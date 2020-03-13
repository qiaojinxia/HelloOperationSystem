;  boot16.asm
;
;  This program is a FAT16 bootloader
;  Copyright (c) 2017-2020, Joshua Riek
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

    %define BOOT_SEG   0x07c0                   ; (BOOT_SEG  << 4)  + BOOT_OFF   = 0x007c00
    %define BOOT_OFF   0x0000
    
    %define STACK_SEG  0x0600                   ; (STACK_SEG  << 4) + STACK_OFF  = 0x007000
    %define STACK_OFF  0x1000

    %define BUFFER_SEG 0x1000                   ; (BUFFER_SEG << 4) + BUFFER_OFF = 0x010000
    %define BUFFER_OFF 0x0000

    %define LOAD_SEG   0x0000                   ; (LOAD_SEG   << 4) + LOAD_OFF   = 0x001000
    %define LOAD_OFF   0x1000

    %ifidn __OUTPUT_FORMAT__, elf               ; WARNING: Assumes that the text segment is set to
      %define BOOT_SEG 0x0000                   ; 0x7c00, used ONLY for debugging with GDB
      %define BOOT_OFF 0x0000
    %endif
    
    bits 16                                     ; Ensure 16-bit code
    cpu  8086                                   ; Assemble with the 8086 instruction set

;---------------------------------------------------
; Disk description table
;---------------------------------------------------

    jmp short entryPoint                        ; Jump over OEM / BIOS param block
    nop

    %define OEMName           bp+0x03           ; Disk label
    %define bytesPerSector    bp+0x0b           ; Bytes per sector
    %define sectorsPerCluster bp+0x0d           ; Sectors per cluster
    %define reservedSectors   bp+0x0e           ; Reserved sectors
    %define fats              bp+0x10           ; Number of fats
    %define rootDirEntries    bp+0x11           ; Number of entries in root dir
    %define sectors           bp+0x13           ; Logical sectors
    %define mediaType         bp+0x15           ; Media descriptor byte
    %define fatSectors        bp+0x16           ; Sectors per FAT
    %define sectorsPerTrack   bp+0x18           ; Sectors per track
    %define heads             bp+0x1a           ; Number of sides/heads
    %define hiddenSectors     bp+0x1c           ; Hidden sectors
    %define hugeSectors       bp+0x20           ; LBA sectors
    %define biosDriveNum      bp+0x24           ; Drive number
    %define reserved          bp+0x25           ; This is not used
    %define bootSignature     bp+0x26           ; Drive signature
    %define volumeId          bp+0x27           ; Volume ID
    %define volumeLabel       bp+0x2b           ; Volume Label
    %define fatTypeLabel      bp+0x36           ; File system type
    
    times 0x3b db 0x00

;---------------------------------------------------
; Start of the main bootloader code and entry point
;---------------------------------------------------
entryPoint:
    jmp BOOT_SEG:$+5                            ; Fix the cs:ip registers
    
bootStrap:
    mov ax, BOOT_SEG                            ; Set segments to the location of the bootloader
    mov ds, ax
    mov es, ax
    
    cli
    mov ax, STACK_SEG                           ; Get the the defined stack segment address
    mov ss, ax                                  ; Set segment register to the bottom  of the stack
    mov sp, STACK_OFF                           ; Set ss:sp to the top of the 4k stack
    sti
    
    mov bp, (0x7c0-STACK_SEG) << 4              ; Correct bp for the disk description table
    push cs
    pop ds
    
    or dl, dl                                   ; When booting from a hard drive, some of the 
    jz loadRoot                                 ; you need to call int 13h to fix some bpb entries

    mov byte [drive], dl                        ; Save boot device number

    mov ah, 0x08                                ; Get Drive Parameters func of int 13h
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    jc loadRoot

    and cx, 0x003f                              ; Maximum sector number is the high bits 6-7 of cl
    mov word [sectorsPerTrack], cx              ; And whose low 8 bits are in ch

    mov dl, dh                                  ; Convert the maximum head number to a word
    xor dh, dh
    inc dx                                      ; Head numbers start at zero, so add one
    mov word [heads], dx                        ; Save the head number

;---------------------------------------------------
; Load the root directory from the disk
;---------------------------------------------------

loadRoot:
    xor cx, cx
    mov ax, 32                                  ; Size of root dir = (rootDirEntries * 32) / bytesPerSector
    mul word [rootDirEntries]                   ; Multiply by the total size of the root directory
    div word [bytesPerSector]                   ; Divided by the number of bytes used per sector
    xchg cx, ax
        
    mov al, byte [fats]                         ; Location of root dir = (fats * fatSectors) + reservedSectors
    mul word [fatSectors]                       ; Multiply by the sectors used
    add ax, word [reservedSectors]              ; Increase ax by the reserved sectors

    mov word [userData], ax                     ; start of user data = startOfRoot + numberOfRoot
    add word [userData], cx                     ; Therefore, just add the size and location of the root directory

    xor dx, dx
    
    mov di, BUFFER_SEG                          ; Set the extra segment to the disk buffer
    mov es, di
    mov di, BUFFER_OFF                          ; Set es:di and load the root directory into the disk buffer
    call readSectors                            ; Read the sectors
   
;---------------------------------------------------
; Find the file to load from the loaded root dir
;---------------------------------------------------
    
findFile:
    mov cx, word [rootDirEntries]               ; Search through all of the root dir entrys for the kernel
    xor ax, ax                                  ; Clear ax for the file entry offset

  searchRoot:
    xchg cx, dx                                 ; Save current cx value to look for the filename
    mov si, filename                            ; Load the filename
    mov cx, 11                                  ; Compare first 11 bytes
    rep cmpsb                                   ; Compare si and di cx times
    je loadFat                                  ; We found the file :)

    add ax, 32                                  ; File entry offset
    mov di, BUFFER_OFF                          ; Point back to the start of the entry
    add di, ax                                  ; Add the offset to point to the next entry
    xchg dx, cx
    loop searchRoot                             ; Continue to search for the file

    mov si, fileNotFound                        ; Could not find the file
    call print

  reboot:
    xor ax, ax
    int 0x16                                    ; Get a single keypress
    
    mov ah, 0x0e                                ; Teletype output
    mov al, 0x0d                                ; Carriage return
    int 0x10                                    ; Video interupt
    mov al, 0x0a                                ; Line feed
    int 0x10                                    ; Video interupt
    int 0x10                                    ; Video interupt

    xor ax, ax
    int 0x19                                    ; Reboot the system
    
;---------------------------------------------------
; Load the fat from the found file   
;--------------------------------------------------

loadFat:
    mov ax, word [es:di+15]                     ; Get the file cluster at offset 26
    push ax                                     ; Store the FAT cluster

    xor ax, ax                                  ; Size of fat = (fats * fatSectors)
    mov al, byte [fats]                         ; Move number of fats into al
    mul word [fatSectors]                       ; Move fat sectors into bx
    mov cx, ax                                  ; Store in cx
    
    xor dx, dx

    mov ax, word [reservedSectors]              ; Convert the first fat on the disk
    mov di, BUFFER_OFF                          ; Set es:di and load the fat sectors into the disk buffer
    call readSectors                            ; Read the sectors

;---------------------------------------------------
; Load the clusters of the file and jump to it
;---------------------------------------------------
    
loadFile: 
    mov di, LOAD_SEG
    mov es, di                                  ; Set es:di to where the file will load
    mov di, LOAD_OFF

    pop ax                                      ; File cluster restored
    call readClusters                           ; Read clusters from the file

    mov dl, byte [drive]                        ; Pass the boot drive into dl
    jmp LOAD_SEG:LOAD_OFF                       ; Jump to the file loaded!

    hlt                                         ; This should never be hit 


;---------------------------------------------------
; Bootloader routines below
;---------------------------------------------------

    
;---------------------------------------------------
readClusters:
;
; Read file clusters, starting at the given cluster,
; expects FAT to be loaded into the disk buffer.
;
; Expects: AX    = Starting cluster
;          ES:DI = Location to load clusters
;
; Returns: None
;
;--------------------------------------------------
    push ax                                     ; NOTE: Dont relly need to push registers here 
    push bx                                     ; just doing this to save space

  .clusterLoop:
    push ax
    
    dec ax
    dec ax
    xor dx, dx
    xor bh, bh                                  ; Get the cluster start = (cluster - 2) * sectorsPerCluster + userData
    mov bl, byte [sectorsPerCluster]            ; Sectors per cluster is a byte value
    mul bx                                      ; Multiply (cluster - 2) * sectorsPerCluster
    add ax, word [userData]                     ; Add the userData

    xor ch, ch
    mov cl, byte [sectorsPerCluster]            ; Sectors to read
    call readSectors                            ; Read the sectors

    xor dx, dx
    pop ax                                      ; Current cluster number

  .calculateNextCluster16:                      ; Get the next cluster for FAT16 (cluster * 2)
    mov bx, 2                                   ; Multiply the cluster by two (cluster is in ax)
    mul bx

  .loadNextCluster:
    push es
    push di

    mov di, BUFFER_SEG
    mov es, di                                  ; Tempararly set es:di to the FAT16 buffer
    mov di, BUFFER_OFF

    clc
    xor cx, cx
    add di, ax                                  ; Add the offset into the FAT16 table
    adc cx, dx                                  ; Add and carry the segment into the FAT16 table

    shl cl, 1                                   ; Shift left 4 bits for the segment 
    shl cl, 1
    shl cl, 1
    shl cl, 1

    mov dx, es
    add dh, cl                                  ; Now we can set the correct segment into the FAT16 table
    mov es, dx

    mov ax, word [es:di]                        ; Load ax to the next cluster in the FAT16 table
    
    pop di
    pop es

  .nextClusterCalculated:
    cmp ax, 0xfff8                              ; Are we at the end of the file?
    jae .done

    xchg cx, ax
    xor dx, dx
    xor bh, bh                                  ; Bytes per cluster = (bytes per sector * sectors per cluster)
    mov ax, word [bytesPerSector]
    mov bl, byte [sectorsPerCluster]
    mul bx
    xchg cx, ax
    
    clc
    add di, cx                                  ; Add to the pointer offset
    jnc .clusterLoop 

  .fixBuffer:                                   ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; extra segment by 0x1000

    jmp .clusterLoop                            ; Load the next file cluster

  .done:
    pop bx
    pop ax

    ret
  
;---------------------------------------------------
readSectors:
;
; Read sectors starting at a given sector by 
; the given times and load into a buffer. Please
; note that this may allocate up to 256KB of ram.
;
; Expects: AX:DX = Starting sector/ lba
;          ES:DI = Location to load sectors
;          CX    = Number of sectors to read
;
; Returns: None
;
;--------------------------------------------------
    push ax
    push bx
    push cx
    push dx
    push di
    push es
    
    mov bx, di                                  ; Convert es:di to es:bx for int 13h

  .sectorLoop:
    push ax
    push cx
    push dx
    
    ;xor dx, dx                                 ; This allows us to access even more sectors!
    
    div word [sectorsPerTrack]                  ; Divide the lba (value in ax:dx) by sectorsPerTrack
    mov cx, dx                                  ; Save the absolute sector value 
    inc cx

    xor dx, dx                                  ; Divide by the number of heads
    div word [heads]                            ; to get absolute head and track values
    mov dh, dl                                  ; Move the absolute head into dh
    
    mov ch, al                                  ; Low 8 bits of absolute track
    shl ah, 1                                   ; High 2 bits of absolute track
    shl ah, 1
    shl ah, 1
    shl ah, 1
    shl ah, 1
    shl ah, 1
    or cl, ah                                   ; Now cx is set with respective track and sector numbers

    mov dl, byte [drive]                        ; Set correct drive for int 13h

    mov di, 5                                   ; Try five times to read the sector
    
  .attemptRead:
    mov ax, 0x0201                              ; Read Sectors func of int 13h, read one sector
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    jnc .readOk                                 ; If no carry set, the sector has been read

    xor ah, ah                                  ; Reset Drive func of int 13h
    int 0x13                                    ; Call int 13h (BIOS disk I/O)
    
    dec di                                      ; Decrease read attempt counter
    jnz .attemptRead                            ; Try to read the sector again

    mov si, diskError                           ; Error reading the disk
    call print
    jmp reboot
    
  .readOk:
    pop dx
    pop cx
    pop ax

    inc ax                                      ; Increase the next sector to read
    add bx, word [bytesPerSector]               ; Add to the buffer address for the next sector
    
    jnc .nextSector 

  .fixBuffer:
    push dx                                     ; An error will occur if the buffer in memory
    mov dx, es                                  ; overlaps a 64k page boundry, when di overflows
    add dh, 0x10                                ; it will trigger the carry flag, so correct
    mov es, dx                                  ; es segment by 0x1000
    pop dx
    
  .nextSector:
    loop .sectorLoop
    
    pop es
    pop di
    pop dx
    pop cx
    pop bx
    pop ax

    ret

;---------------------------------------------------
print:
;
; Print out a simple string.
;
; Expects: DS:SI = String to print
;
; Returns: None
;
;---------------------------------------------------
    lodsb                                       ; Load byte from ds:si to al
    or al, al                                   ; If al is empty stop looping
    jz .done                                    ; Done looping and return
    mov ah, 0x0e                                ; Teletype output
    int 0x10                                    ; Video interupt
    jmp print                                   ; Loop untill string is null
  .done:
    ret

    
;---------------------------------------------------
; Bootloader varables below
;---------------------------------------------------

    diskError      db "Disk err", 0             ; Error while reading from the disk
    fileNotFound   db "File err", 0             ; File was not found
    
    userData       dw 0                         ; Start of the data sectors
    drive          db 0                         ; Boot drive number
    
    filename       db "DEMO    BIN"             ; Filename

                   times 510 - ($ - $$) db 0    ; Pad remainder of boot sector with zeros
                   dw 0xaa55                    ; Boot signature
