;*************************************************************************
;Name: CaoMaoBoy
;E-mail:1158829384@qq.com
;-------------------------------------------------------------------------
;系统启动引导加载程序
;从Fat32 文件系统 加载 启动 OS
;*************************************************************************
[BITS 16]
DirSize	equ	0x08
BaseOfLoader	equ	0x1000
OffsetOfLoader	equ	0x00

BaseOfStack		equ	0xffff
FatPerSector	equ	0x0080

LoderAddress		equ	0x100

SearchFileSector	equ	0x10;搜索多少个扇区目录
org	0x7c00
     jmp	short Fat32Boot_Start	;跳转指令,转到0000:7C3E 3个字节
     nop
     OEM_ID                db 		"CBOS0.01";系统版本号   8个字节
     BytesPerSector        dw 		0x0200;每扇区字节数   2个字节
     SectorsPerCluster     db 		0x01;每簇扇区数 默认 8 1个字节
     ReservedSectors       dw 		0x20;保留扇区个数   2个字节
     ;-------16 bit ⬆️------->
     TotalFATs             db 		0x02;FAT表格数 默认2 1个字节
     MaxRootEntries        dw 		0x00;根目录最多可容纳 目录数 历史字段 FAT32不适用 2个字节
     NumberOfSectors       dw 		0x00;扇区总数 小于32M 在这记录 大于32M 改TotalSectors  2个字节
     MediaDescriptor       db 		0xF8;介质描述 0xF8位硬盘 1个字节
     SectorsPerFAT         dw 		0x00;每个FAT表 的大小扇区数 FAT32 已经不使用了 2个字节
     SectorsPerTrack       dw 		63;每个磁道扇区数 S          ---------->不同硬盘改动 2个字节
     SectorsPerHead        dw 		16;磁头数    H            ---------------->修改 2个字节
     HiddenSectors         dd 		0x00;分区目前已使用 隐藏扇区数 4个字节
     ;-------16 bit ⬆️------->
     TotalSectors     	   dd 		0x11f70;大于32M这里修改 扇区数 ---------------> 4个字节
     BigSectorsPerFAT      dd 		0x0236;每个FAT表使用扇区数  ----------->  4个字节
     Flags                 dw 		0x00;标记 2个字节
     FSVersion             dw 		0x00;版本号 2个字节
     RootDirectoryStart    dd 		0x02;根目录簇号 4个字节 起始于数据区的  + 2个簇 4个字节
     ;-------16 bit ⬆️------->
     FSInfoSector          dw 		0x01;文件系统信息扇区 2个字节
     BackupBootSector      dw 		0x06;备份引导扇区 位于文件系统的6号扇区 2个字节
     TIMES 12              db       0x00;预留12字节未使用
     ;-------16 bit ⬆️------->
     DriveNumber           db 		0x80;驱动器编号 0号设备 1个字节
     ReservedByte          db   	0x00;预留  1个字节
     Signature             db 		0x29;扩展引导标记 1个字节
     VolumeID              dd 		0x00 ;4个字节
     VolumeLabel           db 		"QUASI  BOOT" ;11个字节
     SystemID              db 		"FAT32   ";文件系统 8个字节
     ;-------30 bit ⬆️------->
Fat32Boot_Start:
    mov	ax,	cs
    mov	ds,	ax
    mov	es,	ax
    mov	ss,	ax
    mov	sp,	BaseOfStack
    mov     al, byte [TotalFATs]                ; FAT表个数
    mul     WORD[BigSectorsPerFAT]              ; 每个FAT表大小扇区数 × FAT表个数
    add     ax, WORD [ReservedSectors]          ; 保留扇区 + ↑
    mov     WORD [datasector], ax               ; 数据区 起始位置
;======= 搜索 loader.bin
Lable_Search_In_Root_Dir_Begin:
    cmp	    byte[SectorNo],	SearchFileSector ;找寻 10次
	jz	    Label_No_LoaderBin; 计算每个簇的扇区数
     ;某簇起始LAB逻辑扇区号 = 保留扇区数 + 每个FAT表大小扇区数 × FAT表个数 + (该簇簇号 - 2) × 每簇扇 区数
     ;计算 起始数据簇 的位置 放入寄存器 ax
	mov	    ax,	0x00
	mov	    es,	ax
	mov	    bx,	0x8000
	mov     ax, WORD[RootDirectoryStart]        ;根目录起始簇 FAT一般是 2号簇
	add     al,[SectorNo]
	call    ClusterLBA                             ;2号簇 转换为 FAT所在扇区
	xor     cx,cx
	mov	    cl,byte [SectorsPerCluster]
	call	ReadSectors         ;读取1个扇区
    mov     si,LoaderFileName       ;要寻找的 文件名
    mov	    di,	0x8000
    cld
    mov	dx,	DirSize     ;每个扇区循环 8 次
Label_Search_For_LoaderBin:
    cmp     dx,0
    jz	    Label_Goto_Next_Sector_In_Root_Dir
    dec	    dx              ;每个扇区 8 个 目录
    push    ax
    mov     al,0x41
    cmp     al,byte [es:di]  ;判断 是否以 0x41 开头
    jnz      Label_Different ;不等于 下一次循环
    pop     ax
    add     di,0x20         ;定位到文件名
    mov     cx,11           ;文件名 是一个字节
;加载文件名
Label_Cmp_FileName:
	cmp	    cx,	0            ;循环11次
	jz	    Label_FileName_Found;文件名匹配
	dec	    cx               ;计数器 -1
	lodsb                    ;读入 一个字节 到 al si自动加1
	cmp	    al,	byte[es:di]  ;比较 al文件名 和 es:di指向的一个字节
	jz	    Label_Go_On      ;下一次循环
	jmp	    Label_Different  ;如果  不相等 跳转处理下一个目录的遍历
Label_Go_On:
	inc	di                   ;读取文件名 + 1
	jmp	Label_Cmp_FileName   ;继续for 循环 遍历
Label_Different:
	and	    di,	0xffc0       ;64的倍数
	add	    di,	0x40         ;偏移 64 字节
	mov	    si,	LoaderFileName
	jmp	    Label_Search_For_LoaderBin
Label_No_LoaderBin:
	mov	ax,	1301h
	mov	bx,	008ch
	mov	dx,	0100h
	mov	cx,	21
	push	ax
	mov	ax,	ds
	mov	es,	ax
	pop	ax
	mov	bp,	NoLoaderMessage
	int	10h
	jmp	$
Label_FileName_Found:
    and	    di,	0xffe0        ;32 的倍数 清除后面的位数
    add     di,0x001a         ;指向文件的首簇信息
    mov     si,Fatcluster       ;指向 簇号
    mov     ax,word[es:di-0x06] ;文件簇号的高 2位
    mov     word [si+2],ax           ;高位 存储在 高地址
    mov     ax,word[es:di]      ;读入 低 两个字节
    mov     word [si],ax             ;低位存储在低地址
    jmp     goload
Label_Go_On_Loading_File:
    call    Func_GetFATEntry       ;加载FAT表 第  ax 个 表项 返回 下一个表项 ax
    cmp     word [si + 2],0x0fff;如果 前4位 是 0x0fff 看 后 几位
    jz      Select2
  goload:
    mov     ax, LoderAddress       ; set ES:BX = 0100:0000
    mov     es, ax
    mov     ax,word [si]            ;低16位簇号
    call    ClusterLBA
    xor     cx,cx
    mov     cl,byte [SectorsPerCluster];读取扇区数
    mov     bx,word [Sectoroffset]; set ES:BX = 0100:0000
    call    ReadSectors ;读取一个簇
    mov     word [Sectoroffset],bx; 保存读取完一个扇区后的 偏移 + 512 * n
    jmp     Label_Go_On_Loading_File;todo......
Select2:
    cmp     word [si],0xffff       ;如果  表项 索引 为 0x0fffffff表示 结束
    jz	    Label_File_Loaded   ;文件加载完毕
    jmp     goload

;    mov     bx,[BaseOfLoader]   ;读入文件内容 写入到 0x1000处
;    ;call    ReadSectors
;    jmp     $

Label_File_Loaded:
	jmp	LoderAddress:0000
;读取 rax 个簇 的内容 返回 ax
Func_GetFATEntry:
    mov     si,Fatcluster       ;指向 簇号
    mov     ax,word [si]      ;低16位
    mov     dx,word [si + 2]      ;高16位
    mov     bx,FatPerSector            ;计算扇区
    div     bx   ;dx 扇区内的位置 ax 在哪扇区偏移
    add     ax,[ReservedSectors]             ;添加FAT起始扇区
    mov     bx,0x08000              ;读取到 位置
    mov     cx,1                    ;读取一个扇区
    push    dx
    call    ReadSectors             ; Read the sectors
    pop     dx
    mov     bx,0x08000
    mov     cx,0x04                 ;占用 4个字节
    mov     ax,dx                   ;扇区内位置
    mul     cx                      ;扇区内 位置 * 占用字节
    mov     di,ax                   ;偏移
    mov     ax,word [es:bx+di]         ;低16位
    mov     word [si],ax
    mov     ax,word [es:bx + di + 2]   ;高16位
    mov     word [si+2],ax
    ret


Label_Goto_Next_Sector_In_Root_Dir:
	add	byte    [SectorNo],	1;已搜索扇区数 +1
	jmp	Lable_Search_In_Root_Dir_Begin
     ;*************************************************************************
     ; PROCEDURE 输出显示
     ; display ASCIIZ string at ds:si via BIOS
     ;*************************************************************************
     DisplayMessage:
         lodsb                                       ; load next character
          or      al, al                              ; test for NUL character
          jz      .DONE
          mov     ah, 0x0E                            ; BIOS teletype
          mov     bh, 0x00                            ; display page 0
          mov     bl, 0x07                            ; text attribute
          int     0x10                                ; invoke BIOS
          jmp     DisplayMessage
     .DONE:
          ret
     ;*************************************************************************
     ; PROCEDURE ReadSectors 读取cx个扇区
     ; reads cx sectors from disk starting at ax into
     ;memory location es:bx
     ;*************************************************************************
     ReadSectors:
          push    ax            ;ax = 簇起始LBA逻辑扇区
          push    bx            ;bx = 要写出扇区到内存的地址
          push    cx            ;cx = 要读几个扇区
          call    LBACHS        ;LBA 逻辑扇区 转换为 CHS 格式
          mov     ah, 0x02                            ; 功能号 0x02 读取一个扇区
          mov     al, 0x01                            ; 读入的扇区数
          mov     ch, BYTE [absoluteTrack]            ; track
          mov     cl, BYTE [absoluteSector]           ; sector
          mov     dh, BYTE [absoluteHead]             ; head
          mov     dl, BYTE [DriveNumber]              ; 读入的驱动器号
          int     0x13                                ; 调用中断 读取
          push    si
          mov     si, msgProgress                     ; si 指向待显示字符
          call    DisplayMessage                      ; 调用显示字符  si 指向 地址
          pop     si
          pop     cx                                  ; 恢复 cx = 要读几个扇区
          pop     bx                                  ; 恢复 bx = 要写出扇区到内存的地址
          pop     ax                                  ; 恢复 ax = 簇起始LBA逻辑扇区
          add     bx, WORD [BytesPerSector]           ; bx += 扇区字节数 指向下一个要写出的地址
          inc     ax                                  ; ax += 1  逻辑扇区 +1 下一个被读取扇区
          loop    ReadSectors                         ; cx -1 循环读取一个扇区
          ret
     ;*************************************************************************
     ; PROCEDURE LBACHS
     ; convert ax LBA addressing scheme to CHS addressing scheme
     ; absolute sector = (logical sector / sectors per track) + 1
     ; absolute head   = (logical sector / sectors per track) MOD number of heads
     ; absolute track  = logical sector / (sectors per track * number of heads)
     ;*************************************************************************
     LBACHS:
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [SectorsPerTrack]              ; calculate
          inc     dl                                  ; adjust for sector 0
          mov     BYTE [absoluteSector], dl
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [SectorsPerHead]                     ; calculate
          mov     BYTE [absoluteHead], dl
          mov     BYTE [absoluteTrack], al
          ret
     ;*************************************************************************
     ; PROCEDURE ClusterLBA
     ; 转换 FAT 表项 在数据区 内的索引
     ; FileStartSector = ((X − 2) * SectorsPerCluster(0x08))
     ;*************************************************************************
     ClusterLBA:
          sub     ax, 0x0002                          ; zero base cluster number
          xor     cx, cx
          mov     cl, BYTE [SectorsPerCluster]        ; convert byte to word
          mul     cx
          add     ax, WORD [datasector]               ; base data sector
          ret
     Sectoroffset   dw 00
     Fatcluster dd 0x00
     NoLoaderMessage:	db	"X"
     SectorNo db 0x00
     LoaderFileName: db	"LOADER  BIN",0
     msgProgress db ".", 0x00
     StartBootMessage:	db	"Y"
     absoluteSector db 0x00;S
     absoluteHead   db 0x00;H
     absoluteTrack  db 0x00;C
     datasector  dw 0x0000
     TIMES 510-($-$$) DB 0
     DW 0xAA55