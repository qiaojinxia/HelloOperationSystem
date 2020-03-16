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
	mov		ax,	1301h
	mov		bx,	000fh
	mov		dx,	0000h
	mov		cx,	10
	push	ax
	mov		ax,	ds
	mov		es,	ax
	pop		ax
	mov		bp,	StartBootMessage
	int		10h
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
  xor	    ax,	ax
  mov	    es,	ax
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
  jmp     	goload
Label_File_Loaded:
	jmp	loder_begin
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
loder_begin:
	mov		ax,cs
	mov 	es,ax
	mov 	ds,ax
	mov 	ss,ax
	mov 	sp,BaseOfStack
	;加载GDT
	lgdt	[GDTPtr]

	;关闭外部中断
	cli
	;开启A20 快速门
	in      al,0x92
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
LoaderESAddress dw 0x7000
Sectoroffset   dw 00
Fatcluster dd 0x00
NoLoaderMessage:	db	"Can't find you kernal!"
SectorNo db 0x00
LoaderFileName: db	"KERNEL  BIN",0;寻找的 文件名 loader.bin
     msgProgress db ".", 0x00
     StartBootMessage:	db	"Start Loading Kernel.bin Please Waiting"
     absoluteSector db 0x00;S
     absoluteHead   db 0x00;H
     absoluteTrack  db 0x00;C
     datasector  dw 0x0000	

[SECTION .code32]
[BITS 32]
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
  call  showMSG
  jmp   $
showMSG:
  mov   edi,(80*11);屏幕中间位置
  add   edi,ebx;自带符偏移
  mov   eax,edi
  mul   ecx ;每个字符 2个字节 所以乘以2
  mov   edi,eax
  mov   ah,0xc
  mov   al,'L';位置
  inc    ebx
  mov   [gs:edi],ax ;写入屏幕
  mov   al,'o';位置
  inc    ebx
  mov   [gs:edi+2],ax ;写入屏幕
  mov   al,'a';位置
  inc     ebx
  mov   [gs:edi+4],ax ;写入屏幕
  mov   al,'d';位置
  inc     ebx
  mov   [gs:edi+6],ax ;写入屏幕
  mov   al,'e';位置
  inc     ebx
  mov   [gs:edi+8],ax ;写入屏幕
  mov   al,'d';位置
  inc    ebx
  mov   [gs:edi+10],ax ;写入屏幕
  ret
fin:
  HLT
  jmp   fin
[SECTION .data32]
[BITS 32]
align 32
DATA32:	
BottomOfStack times 0x100 db 0
TopOfStack equ $ + LOADER_PHY_ADDR





	