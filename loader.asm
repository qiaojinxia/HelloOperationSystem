org 0x100
jmp clear_display
%include "struct.inc"
%include "fat32hd.inc"
%include "cf.inc"
;============================================================================
;   GDT全局描述符表相关信息以及堆栈信息
;----------------------------------------------------------------------------
; 描述符                        基地址        段界限       段属性
LABEL_GDT:          GDT_Descriptor      0,            0,          0             ; 空描述符，必须存在，不然CPU无法识别GDT
LABEL_DESC_CODE:    GDT_Descriptor    0,        0xfffff,    DA_32 | DA_CR | DA_LIMIT_4K ; 0~4G，32位可读代码段，粒度为4KB
LABEL_DESC_DATA:    GDT_Descriptor  0, 		  0xfffff,    DA_32 | DA_DRW | DA_LIMIT_4K; 0~4G，32位可读写数据段，粒度为4KB
LABEL_DESC_VIDEO:   GDT_Descriptor  0xb8000,  0xfffff,    DA_DRW | DA_DPL3            ; 视频段，特权级3（用户特权级）
; GDT全局描述符表 -------------------------------------------------------------
GDTLen              equ $ - LABEL_GDT                           ; GDT的长度
GDTPtr              dw GDTLen - 1                               ; GDT指针.段界限
                    dd LOADER_PHY_ADDR + LABEL_GDT              ; GDT指针.基地址
; GDT选择子 ------------------------------------------------------------------
SelectorCode        equ LABEL_DESC_CODE - LABEL_GDT             ; 代码段选择子
SelectorData        equ LABEL_DESC_DATA - LABEL_GDT             ; 数据段选择子
SelectorVideo       equ LABEL_DESC_VIDEO - LABEL_GDT | SA_RPL3  ; 视频段选择子，特权级3（用户特权级）
; GDT选择子 ------------------------------------------------------------------
clear_display:
	mov	    ax,	cs
  mov	    ds,	ax
  mov	    es,	ax
  mov	    ss,	ax
  mov	    sp,	BaseOfStack
	;=======	clear screen
	mov		ax,	0600h
	mov		bx,	0700h
	mov		cx,	0
	mov		dx,	0184fh
	int		10h

;=======	set focus

	mov		ax,	0200h
	mov		bx,	0000h
	mov		dx,	0000h
	int		10h

;=======	display on screen : Start Booting......
	mov		si,	StartBootMessage
  call  DisplayMessage
Fat32Boot_Start:
	  xor		  ax,ax
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
  mov	    bx,	0x8000
  mov     ax, WORD[RootDirectoryStart]        ;根目录起始簇 FAT一般是 2号簇
  add     al,[SectorNo]
  call    ClusterLBA                             ;2号簇 转换为 FAT所在扇区
  xor     cx,cx
  mov	    cl,byte [SectorsPerCluster]
  call	  ReadSectors         ;读取1个扇区
  mov     si,LoaderFileName       ;要寻找的 文件名
  mov	    di,	0x8000
  cld
  mov	    dx,	DirSize     ;每个扇区循环 8 次
Label_Search_For_LoaderBin:
  cmp     dx,0
  jz	    Label_Goto_Next_Sector_In_Root_Dir
  dec	    dx              ;每个扇区 8 个 目录
  push    ax
  mov     al,0x41
  cmp     al,byte [es:di]  ;判断 是否以 0x41 开头
  jnz     Label_Different ;不等于 下一次循环
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
	inc	    di                   ;读取文件名 + 1
	jmp	    Label_Cmp_FileName   ;继续for 循环 遍历
Label_Different:
	and	    di,	0xffc0       ;64的倍数
	add	    di,	0x40         ;偏移 64 字节
	mov	    si,	LoaderFileName
	jmp	    Label_Search_For_LoaderBin
Label_No_LoaderBin:
	mov	    si,	NoLoaderMessage
  call    DisplayMessage
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
  mov     ax, word [LoaderESAddress]       ; set ES:BX = 0100:0000
  mov     es, ax
  mov     ax,word [si]            ;低16位簇号
  call    ClusterLBA
  xor     cx,cx
  mov     cl,byte [SectorsPerCluster];读取扇区数
  mov     bx,word [Sectoroffset]; set ES:BX = 0100:0000
  call    ReadSectors ;读取一个簇
  cmp     bx,0x1100   ;每 4096个 bx  ax 进位 0x100
  jz      ReadSectorOffset
Select1:  
  mov     word [Sectoroffset],bx; 保存读取完一个扇区后的 偏移 + 512 * n
  jmp     Label_Go_On_Loading_File;todo......
Select2:
  cmp     	word [si],0xffff       ;如果  表项 索引 为 0x0fffffff表示 结束
  jz	    Label_File_Loaded   ;文件加载完毕
  jmp     goload
Label_File_Loaded:
	jmp     Loder_begin
;读取 rax 个簇 的内容 返回 ax
Func_GetFATEntry:
  mov     si,Fatcluster       ;指向 簇号
  mov     ax,word [si]      ;低16位
  mov     dx,word [si + 2]      ;高16位
  mov     bx,FatPerSector            ;计算扇区
  div     bx   ;dx 扇区内的位置 ax 在哪扇区偏移
  add     ax,[ReservedSectors]       ;添加FAT起始扇区
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
;最大支持 写出1M空间
ReadSectorOffset:
  mov     ax, word [LoaderESAddress]
  add     ax,0x100 
  mov     word [LoaderESAddress],ax
  mov     bx,0x100
  jmp     Select1

Label_Goto_Next_Sector_In_Root_Dir:
	add	byte    [SectorNo],	1;已搜索扇区数 +1
	jmp	Lable_Search_In_Root_Dir_Begin
     ;*************************************************************************
     ; PROCEDURE 输出显示
     ; display ASCIIZ string at ds:si via BIOS
     ;*************************************************************************
     DisplayMessage:
          push    ax
          push    bx
      .startdisplay:
          lodsb                                       ; load next character
          cmp     al, 0x00                              ; test for NUL character
          jz      .DONE
          mov     ah, 0x0E                            ; BIOS teletype
          mov     bx, 0x000f
          int     0x10                                ; invoke BIOS
          jmp     .startdisplay
     .DONE:
          pop     bx
          pop     ax
          ret
     ;*************************************************************************
     ; PROCEDURE ReadSectors 
     ; 从 ax LBA逻辑扇区 
     ; 读取 cx 个 扇区 写入 es:bx 内存处 
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
     ; 将 LBA 逻辑扇区 转换为 FAT 表 内所在的逻辑扇区
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
          ret        ; base data sector          ret


;=============================================
;
;         16位模式下开启保护模式
;         
;
;=============================================
Loder_begin:
	mov		ax,cs
	mov 	es,ax
	mov 	ds,ax
	mov 	ss,ax
	mov 	sp,BaseOfStack

  ;得到BIOS检查的内存信息
  mov   ebx,0
  mov   di,_MemCkBuf
MemChkLoop:
  mov   eax,0x0000e820
  mov   ecx,20    ;ecx 的ARDS字节大小
  mov   edx,0x0534d4150
  int   0x15
  jc    MemChkFail;判断 CF 标志位 是不是等于0 CF= 1出错
  add   di,20     ;指向存放Adres缓冲区 下一个ARDS起始位置
  inc   dword [_ddMCRCount]
  cmp   ebx,0
  je    MemChkSucc
  jmp   MemChkLoop
MemChkFail:
    mov   dword [_ddMCRCount],0 ;检查失败 ARDS 数量为 0
    mov   si,MMCheckFA
    call  DisplayMessage
    mov  si,nextline
    call  DisplayMessage
    jmp   Load_GBT
MemChkSucc:
    mov  si,nextline
    call  DisplayMessage
    mov  si,MMCheckSS
    call  DisplayMessage
    mov  si,nextline
    call  DisplayMessage

Load_GBT:
	;加载GDT
	lgdt	[GDTPtr]
	;关闭外部中断
	cli
	;开启A20 快速门
	in    al,0x92
  or 		al, 00000010b
  out 	92h, al
	;设置cr0 开启保护模式
	mov 	eax,cr0
	or 		eax,0x1
	mov		cr0,eax
  jmp 	dword SelectorCode:LOADER_PHY_ADDR + segment32
  jmp 	$
;关闭驱动器
KillMotor:
	push	dx
	mov		dx,03F2h
	mov		al,0
	out		dx,al
	pop		dx
	ret


LoaderESAddress: dw 0x7000
Sectoroffset:   dw 00
Fatcluster: dd 0x00
NoLoaderMessage:	db	"Can't find you kernal!",0
SectorNo: db 0x00
LoaderFileName: db	"KERNEL  BIN";寻找的 文件名 loader.bin
     msgProgress: db ".",0
     nextline:    db  0x0d,0x0a,0
     StartBootMessage:	db	"Start Loading Kernel.bin Please Waiting",0
     MMCheckSS:  db  "Memory Check Successful! ",0
     MMCheckFA:  db  "Memory Check Fail! ",0
     absoluteSector: db 0x00;S
     absoluteHead:   db 0x00;H
     absoluteTrack:  db 0x00;C
     datasector:  dw 0x0000	

[SECTION .code32]
[BITS 32]
align 32
segment32:
  mov   ax, SelectorData
  mov   ds, ax
  mov   es, ax
  mov   fs, ax
  mov   ss, ax              ; ds = es = fs = ss = 数据段
  mov   esp, TopOfStack     ; 设置栈顶
  mov   ax, SelectorVideo
  mov   gs, ax              ; gs = 视频段
  mov   ebx,0x10;列偏移多少个字符
  mov   ecx,2;一个字符 2个字节 所以乘以2
  call  CalcMemory
  push  LoadMseeage;保存字符串指针
  call  Print
  add   esp,4 ;清理 字符串指针
  call  PrintMemSize
fin:
  HLT
  jmp   fin
;============================
;  获取可用内存
;
;=============================

CalcMemory:
  push  esi
  push  ecx
  push  edx
  push  edi
  mov  esi,MemCkBuf ;拷贝源
  mov  ecx,[ddMCRCount];循环次数
.loop:
  mov  edx,5
  mov  edi,ARDS;被拷贝地址
.s1:
  push  dword [esi];将 esi处的2个字节压栈
  pop   eax   ;取出
  stosd       ;将ds:eax 取出 赋值到 edi处
  add   esi,4 ;edi指针 加 2个字节 指向下一个待读字节
  dec   edx   ;i --
  cmp   edx,0 ;判断循环结束尾
  jnz   .s1     ;循环没有结束
  cmp   dword [ddType],1
  jnz   .s2
  ;计算 OS 可用内存范围
  mov   eax,[ddbaseAddrLow]  ;起始地址
  add   eax,[ddLengthLow]   ;长度 32位 系统 表示 4G 所以不需要 ddLengthHight
  cmp   eax,[ddMemSize]
  jb    .s2
  mov   dword[ddMemSize],eax
.s2:
  loop  .loop
  pop  edi
  pop  edx
  pop  ecx
  pop  esi
  ret
;============================================================================
;   显示一个整形数
;----------------------------------------------------------------------------
PrintInt:
  mov ah, 0Fh     ; 0000b: 黑底    1111b: 白字
  mov al, '0'
  push  edi
  mov edi, [ddDispPosition]
  mov [gs:edi], ax
  add edi, 2
  mov al, 'x'
  mov [gs:edi], ax
  add edi, 2
  mov [ddDispPosition], edi ; 显示完毕后，设置新的显示位置
  pop edi

  mov eax, [esp + 4]
  shr eax, 24
  call  PrintAl

  mov eax, [esp + 4]
  shr eax, 16
  call  PrintAl

  mov eax, [esp + 4]
  shr eax, 8
  call  PrintAl

  mov eax, [esp + 4]
  call  PrintAl
  ret
 ;============================================================================
;   显示 AL 中的数字
;----------------------------------------------------------------------------
PrintAl:
  push ecx
  push edx
  push edi
  push eax

  mov edi, [ddDispPosition] ; 得到显示位置

  mov ah, 0Fh   ; 0000b: 黑底 1111b: 白字
  mov dl, al
  shr al, 4
  mov ecx, 2
.begin:
  and al, 01111b
  cmp al, 9
  ja  .1
  add al, '0'
  jmp .2
.1:
  sub al, 10
  add al, 'A'
.2:
  mov [gs:edi], ax
  add edi, 2

  mov   al, dl
  loop  .begin
  mov   [ddDispPosition], edi ; 显示完毕后，设置新的显示位置
  pop   eax
  pop   edi
  pop   edx
  pop   ecx

  ret 
;============================
;  打印函数
;
;=============================
Print:
  push  esi
  push  edi
  push  ebx
  push  ecx
  push  edx
  mov   esi,[esp + 0x18] ;指向栈向前第六个指针 字符串指针
  mov   edi,[ddDispPosition] ;输出起始位置
  mov   ah,0xf ;白底黑字
.s1:
  lodsb
  test  al,al
  jz    .closePrint
  cmp   al,10 ;换行符
  jz    .s2
  mov   [gs:edi],ax;往屏幕打印
  add   edi,2  ;下一列
  jmp   .s1
.s2: ; 处理换行符 '\n'
  push  eax
  mov   eax,edi
  mov   bl,160    ;每一行 80个字符 一个字符栈2个字节 所以 =160字节
  div   bl       ;计算当前行的下一行
  inc   eax
  mov   bl,160
  mul   bl       ;将下一行 乘以 每列字数  计算出 下一行起始位置
  mov   edi,eax  ;指向 
  pop   eax
  jmp   .s1
.closePrint:
  mov dword [ddDispPosition], edi ; 打印完毕，更新显示位置
  pop   edx
  pop   ecx
  pop   ebx
  pop   edi
  pop   esi
  ret
PrintMemSize:
  push  ebx
  push  ecx
  mov   eax,[ddMemSize]
  xor   edx,edx
  mov   ebx,1024
  div   ebx
  push  eax
  push  strMemSize
  call  Print
  add   esp,4
  call  PrintInt
  add   esp,4
  push  strMemtype
  call  Print
  add   esp,4
  pop   ecx
  pop   ebx
  ret

[SECTION .data32]
[BITS 32]
align 32
DATA32:
_ddMCRCount:   dd  0    ;BIOS检查 提供内存信息
_ddMemSize:    dd  0    ;内存大小
_ddDispPosition:   dd  (80 * 2 + 0) * 2 ;从第三行开始输出
_ARDS:
  _ddbaseAddrLow:   dd  0;地址 低 32位
  _ddbaseAddrHigh:  dd  0;基地址高32位
  _ddLengthLow:     dd  0;内存长度(byte) 低32位
  _ddLengthHigh:    dd  0;内存长度(字节)高 32 位
  _ddType:          dd  0;ARDS的类型,用于判断是否可以被OS使用

_MemCkBuf:   times 256 db 0 ;存放 由BIOS提供 的内存检查ARDS结构信息,对齐 256 byte/ 20byte = 12 个ARDS结构

_strMemSize:  db "Memory Size:",0
_strMemtype:  db "MB",10,0

_LoadMseeage: DB "Welcome To 32Bits Protect Model ! (#^.^#)",10,0

BottomOfStack times 0x1000 db 0
TopOfStack equ $ + LOADER_PHY_ADDR

ddDispPosition   equ   LOADER_PHY_ADDR + _ddDispPosition
ddMCRCount  equ   LOADER_PHY_ADDR + _ddMCRCount
ddMemSize  equ   LOADER_PHY_ADDR + _ddMemSize
ARDS  equ   LOADER_PHY_ADDR + _ARDS
  ddbaseAddrLow  equ   LOADER_PHY_ADDR + _ddbaseAddrLow
  ddbaseAddrHigh  equ   LOADER_PHY_ADDR + _ddbaseAddrHigh
  ddLengthLow  equ   LOADER_PHY_ADDR + _ddLengthLow
  ddLengthHigh  equ   LOADER_PHY_ADDR + _ddLengthHigh
  ddbaseAddrLow  equ   LOADER_PHY_ADDR + _ddbaseAddrLow
  ddType  equ   LOADER_PHY_ADDR + _ddType
MemCkBuf   equ   LOADER_PHY_ADDR + _MemCkBuf
LoadMseeage  equ LOADER_PHY_ADDR + _LoadMseeage

strMemSize   equ   LOADER_PHY_ADDR + _strMemSize
strMemtype  equ LOADER_PHY_ADDR + _strMemtype

;--------------------------------------





	