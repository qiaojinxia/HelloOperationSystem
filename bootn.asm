;====================================================================
;
; FlyingDragon OS Boot Sector FOR FAT32
;
; Author: Jack
; V0.01 2005-07-23 17:39
; V0.02 2005-08-07 09:29
;
; Build : nasm -f bin FAT32.ASM -oFAT32.BIN
;
;====================================================================
;
; BIOS在启动中的角色:
;  (1) BIOS装载引导驱动器上的0扇区(CHS = 0:0:1)内容到内存线性地址7C00H处；
;  (2) BIOS检查所装载的扇区是否有启动标记(510、511字节分别为55H和AAH)；
;  (3) CPU寄存器DL被设置为分配给引导驱动器的驱动器号，00H为软驱A，80H为硬盘C；
;  (4) BIOS跳转到其装载的扇区中的代码(即7C00H处)，将控制权转交给引导代码。
;
; 引导代码应该初始化以下寄存器：
;  (1) DS：某些BIOS设置其值为0，某些设置其为40H，它应该被设置为(7C00H-BOOT_ORG)/16；
;   其中，BOOT_ORG为引导代码的ORG值，该值通常为7C00H(这意味着DS应设置为0);
;  (2) SS和SP(堆栈)：这两个寄存器的初始值依赖于BIOS；
;  (3) CS个IP(通过JMP指令)：大多数的BIOS进入启动代码的地址为0000:7C00H，但是某些
;   BIOS却跳转到07C0:0000H。由于短跳转和条件跳转是IP相关的，因此如果没有使用
;  远跳转或者绝对跳转，则不需要重置CS和IP；然而，DS仍旧必须是正确的值。
;
;=====================================================================
;
; 常规内存( 0000 0000H - 000F FFFFH，即0-1MB )在系统启动时的使用情况
;
;=====================================================================
;
;   ---------------------------------
;   | 0000 0000 - 0000 03FF    | 1024B   IDT  read only
;   |-------------------------------|
;   | 0000 0400 - 0000 04FF  | 256B  BIOS Data Area , read only
;   |-------------------------------|
;   | 0000 0500 - 0000 7BFF   |* 30464B Free Memory , read/write  (29.75KB)
;   |------------------------------ |
;   | 0000 7C00 - 0000 7DFF    | 512B Boot Sector , read/write
;   |------------------------------ |
;   | 0000 7E00 - 0000 7FFF  | 512B    Free Memory , read /write
;   |------------------------------ |
;   | 0000 8000 - 0009 FBFF   | 607KB Free Memory , read / write( 32K - 639KB )
;   |------------------------------ |
;   | 0009 FC00 - 0009 FFFF    |** 1KB  EBDA extended BIOS data area
;   |------------------------------ |
;   | 000A 0000 - 000A FFFF  | 64KB Video Memory
;   |------------------------------ |
;   | 000B 0000 - 000B 7FFF   | 32KB Mono Video Text Memory
;   |------------------------------ |
;   | 000B 8000 - 000B FFFF    | 32KB Color Video Text Memory
;   |------------------------------ |
;   | 000C 0000 - 000C 7FFF  | 32KB Video BIOS , read only
;   |------------------------------ |
;   | 000C 8000 - 000E FFFF   | 160KB Adapter ROM，read only
;   |------------------------------ |
;   | 000F 0000 - 000F FFFF    | 64KB System BIOS, read only
;   |------------------------------ |
;   | 0010 0000 - 0010 FFEF  |***64KB-16 High Memory Area,read/write  ( 1MB开始处 )
;   |------------------------------ |
;   | 0010 FFF0 -      |   Free Extended Memory, read/write
;   |------------------------------ |
;
;  * 空闲内存实际并非从 0000 0500处开始，BIOS数据区实际上会利用从0000 0500开始的少量字节，例如
;   00000500处保存的是打印屏幕状态，当按下打印屏幕(PrintScreen)键时，低级键盘BIOS初始化打印屏
;   幕功能，键盘BIOS触发中断5打印屏幕处理程序。正因为BIOS数据区越过了256B的界限，因此DOS实际
;   上是从0000 0522开始装载的。为保险起见，可从0000 0600开始利用空闲内存。（1.5K - 31K 29.5KB)
;
; ** 有些机器上没有这段BIOS扩展数据区。
;
; *** 如果没有使用扩展高端内存区域程序（例如Emm386.exe)，则从0010 0000 (1MB )开始的内存都是可用的。
;
;
;===================================================================
;

BITS   16   ; 生成16位代码而不是32位代码
SECTION  .TEXT   ; 代码段
ORG   7C00H  ; 指定程序被装入内存的起始位置

;===================================================================
;
; 宏和常量定义
;
;===================================================================
?     EQU  0  ; NASM不支持DW ?这样的语法，可以使用这样的定义
        ; 模拟，以使代码的可读性更强
DATA_BUF_SEG EQU  0200H ; 用于读取根目录或文件内容的缓冲区(8K) 段地址
DATA_BUF_OFF EQU  2000H
STACK_ADDR  EQU  7BD0H ; 堆栈栈顶(注意：堆栈大小约为20K)
OSLOADER_ADDR EQU  8000H ; FDOSLDR.BIN放入内存中的起始位置，这就意味着
        ; 装载程序及相关资源的尺寸不能超过608K
        ; 8000H - A000H (32K - 640K )
OSLOADER_SEG EQU  0800H ; 起始段地址
SECOND_SECTOR EQU  03H  ; 第二个引导扇区的扇区号(第四个扇区)
SECOND_ADDR  EQU  7E00H ; 第二个引导扇区的装载位置

;====================================================================
; 用堆栈保存若干中间变量( SS = 0 BP = 7C00H )
;====================================================================
FAT_START_SECTOR EQU  4  ; FAT表的起始扇区号  DWORD
ROOT_START_SECTOR EQU  8  ; 根目录的起始扇区号 DWORD
DATA_START_SECTOR EQU  12  ; 数据区起始扇区号  DWORD
FAT_ENTRY_SECTORS EQU  14  ; FAT表所占的扇区数  WORD
ROOT_ENTRY_SECTORS EQU  16  ; 根目录所占的扇区数 WORD
DIR_PER_SECTOR  EQU  17  ; 每个扇区所容纳的目录 BYTE
DISK_EXT_SUPPORT EQU  18     ; 磁盘是否支持扩展BIOS BYTE
CURRENT_CLUSTER  EQU  40  ; 当前正在处理的簇号 DWORD


;====================================================================
; 扩展磁盘服务所使用的地址包
;====================================================================
DAP_SECTOR_HIGH  EQU  24  ; 起始扇区号的高32位 ( 每次调用需要重置 ) DWORD
DAP_SECTOR_LOW  EQU  28  ; 起始扇区号的低32位 ( 每次调用需要重置 ) DWORD
DAP_BUFFER_SEG  EQU  30  ; 缓冲区段地址   ( 每次调用需要重置 ) WORD
DAP_BUFFER_OFF  EQU  32  ; 缓冲区偏移   ( 每次调用需要重置 ) WORD
DAP_RESERVED2  EQU  33  ; 保留字节
DAP_READ_SECTORS EQU  34  ; 要处理的扇区数(1 - 127 )
DAP_RESERVED1  EQU  35  ; 保留字节
DAP_PACKET_SIZE  EQU  36  ; 包的大小为16字节

;====================================================================
;
; 目录项结构(每个结构为32字节)
;
;====================================================================
OFF_DIR_NAME    EQU  0  ; 目录项的偏移  BYTE[11]
OFF_DIR_ATTRIBUTE   EQU  11  ; 目录属性   BYTE
OFF_NT_RESERVED    EQU  12  ; 保留属性   BYTE
OFF_CREATE_TIME_HUNDREDTH EQU  13  ; 创建时间   BYTE
OFF_CREATE_TIME    EQU  14  ; 创建时间   WORD
OFF_CREATE_DATE    EQU  16  ; 创建时间   WORD
OFF_LAST_ACCESS_DATE  EQU  18  ; 上次访问时间  WORD
OFF_START_CLUSTER_HIGH  EQU  20  ; 起始簇号高位  WORD
OFF_LAST_UPDATE_TIME  EQU  22  ; 上次更新时间  WORD
OFF_LAST_UPDATE_DATE  EQU  24  ; 上次更新时间  WORD
OFF_START_CLUSTER_LOW  EQU  26  ; 起始簇号低位  WORD
OFF_FILE_SIZE    EQU  28  ; 文件尺寸   DWORD

; 相关常量
DIR_NAME_DELETED   EQU  0E5H ; 该项已经被删除
DIR_NAME_FREE    EQU  00H  ; 该项是空闲的(其后也是空闲的)
DIR_NAME_DOT    EQU  2EH  ; 特殊目录 . 或 ..
DIR_NAME_SPACE    EQU  20H  ; 不允许的字符
DIR_ENTRY_SIZE    EQU  32  ; 每个目录项的尺寸，其结构如上所示

;文件属性
DIR_ATTR_READONLY   EQU  01H  ; 只读文件
DIR_ATTR_HIDDEN    EQU  02H  ; 隐藏文件
DIR_ATTR_SYSTEM    EQU  04H  ; 系统文件
DIR_ATTR_VOLUME    EQU  08H  ; 卷标号(只可能出现在根目录中)
DIR_ATTR_SUBDIR    EQU  10H  ; 子目录
DIR_ATTR_ARCHIVE   EQU  20H  ; 归档属性
DIR_ATTR_LONGNAME   EQU  0FH  ; 长文件名
DIR_ATTR_LONGNAME_MASK  EQU  3FH  ; 长文件名掩码

; 簇属性
CLUSTER_MASK    EQU  0FFFFFFFH ; 簇号掩码(FAT32=>FAT28)
CLUSTER_FREE    EQU  00000000H ; 簇是空闲的
CLUSTER_RESERVED   EQU  00000001H ; 簇是保留的
CLUSTER_MIN_VALID   EQU  00000002H ; 最小有效簇号
CLUSTER_MAX_VALID   EQU  0FFFFFF6H ; 最大有效簇号
CLUSTER_BAD     EQU  0FFFFFF7H ; 坏簇
CLUSTER_LAST    EQU  0FFFFFF8H   ;0xFFFFFFF8-0xFFFFFFFF表示文件的最后一个簇

;====================================================================
;
; 启动扇区(512字节)
;
;====================================================================
_ENTRY_POINT:

			; 3字节的跳转指令
 JMP SHORT _BOOT_CODE 	; 跳转到真正的引导代码
 NOP      		; 空指令以保证字节数为3

			; 8字节的OEMName
OEMName     DB "FDOS1.00"

;====================================================================
;
; BPB( BIOS Parameter Block )
;
;====================================================================
BytesPerSector   DW ? 	; 每个扇区的字节数 (512 1024 2048 4096)
SectorsPerCluster  DB ? ; 每个簇的扇区数 ( 1 2 4 8 16 32 64 128 )
        		; 两者相乘不能超过32K(簇最大大小)
ReservedSectors   DW ?	; 从卷的第一个扇区开始的保留扇区数目；
        		; 该值不能为0，对于FAT12/FAT16，该值通常为1；
        		; 对于FAT32，典型值为32；
NumberOfFATs   DB ? 	; 卷上FAT数据结构的数目，该值通常应为2
RootEntries    DW ? 	; 对于FAT12/FAT16,该值表示32字节目录项的数目；
        		; 对于FAT32，该值必须为0；
NumberOfSectors16  DW ? ; 该卷上的扇区总数，该字段可以为0，如果该字段
        		; 为0，则NumberOfSectors32不能为0；对于FAT32，
        		; 该字段必须为0
MediaDescriptor   DB ? 	; 介质类型
SectorsPerFAT16   DW ? 	; 该字段标识一个FAT结构占有的扇区数（FAT12/FAT16），
        		; 对于FAT32卷，该字段必须为0；
SectorsPerTrack   DW ? 	; 用于INT 0x13中断的每个磁道的扇区数
HeadsPerCylinder  DW ? 	; 用于INT 0x13中断的每个柱面的磁头数
HiddenSectors   DD ? 	; 包含该FAT卷的分区之前的隐藏扇区数
NumberOfSectors32  DD ? ; 该字段包含该卷上的所有扇区数目，对于FAT32，该字段
        		; 不为0；FAT12/FAT16可根据实际大小是否超过65536个扇
        		; 区数决定是否采用该字段；

;====================================================================
;
; EBPB ( Extended BIOS Parameter Block )
;
;====================================================================
SectorsPerFAT32   DD ?  ; 对于FAT32，该字段包含一个FAT的大小，而SectorsPerFAT16
			; 字段必须为0;
ExtFlags    DW ?   	; 标志
FSVersion    DW ?   	; 这是文件系统的版本，高字节为主版本，低字节为次版本；
RootDirectoryStart  DD ?; 根目录的起始簇号，通常为2；
FSInfoSector   DW ?   	; FSINFO结构在FAT32卷保留区域的扇区号
BackupBootSector  DW ?  ; 如果该字段不为0，则表示在保留区域保存的启动记录的扇区号
          		; 通常为6，不推荐其他值；
Reserved1 TIMES 12 DB ? ; 保留字段，格式化程序应将该字段清0
DriveNumber    DB ?   	; 用于INT 0x13的驱动器号，0x00为软盘，0x80为硬盘
NTReserved    DB ?   	; 保留字节（用于Windows NT，即NTFS），对于FAT文件
         		; 系统，应始终为0
BootSignature   DB  29H ; 扩展引导标志（值为0x29）
VolumeId    DD ?   	; 卷的序列号
VolumeLabel  times 11   DB ?  ; 卷标号，该字段与根目录中的11字节卷标相同
         		; NASM目前尚不支持DUP语法
FileSystemType   DB 'FAT32   ' ; 文件系统类型

;====================================================================
;
; 真正的启动代码从这开始( 偏移：0x3E )
; 其功能是搜索磁盘的根目录，查找FDOSLDR.BIN文件，将其读入内存并运行。
;
;====================================================================
_BOOT_CODE:

 ; 初始化相关寄存器及标志位
 CLI      ; 先关掉中断
 CLD      ; 方向为向前递增
 XOR  AX,AX   ; AX = 0
 MOV  DS,AX   ; 设置数据段寄存器 DS:SI
 MOV  ES,AX   ; 设置附加段寄存器 ES:DI
 MOV  SS,AX   ; 设置堆栈段寄存器
 MOV  BP,7C00H  ; 设置基址寄存器
 MOV  SP,STACK_ADDR ; 设置堆栈栈顶
 STI      ; 允许中断

 ;====================================================================
 ; 保存启动的磁盘编号
 ;====================================================================
 MOV  [DriveNumber],DL; 该值由BIOS设置，如果是从USB启动，该值为0x80
       ; 即为第一个硬盘的编号，该值将用于后续的磁盘
       ; 读取调用


 ;====================================================================
 ; 准备FAT32文件系统常用的常数，以便后面的操作
 ;====================================================================
 ;
 ; [隐藏扇区][保留扇区][FAT][DATA]
 ;
 ;====================================================================

 ;====================================================================
 ; 检查是否支持磁盘中断INT 13H的扩展
 ;====================================================================
 MOV  BYTE [BP - DISK_EXT_SUPPORT],00H  ; 00H表示不支持磁盘扩展
 MOV  DL,[DriveNumber]
 MOV  AH,41H
 MOV  BX,055AAH
 INT  13H
 JC  _NO_DISK_EXTENSION      ; 如果失败，进位标志为1或者BX值不对( AA55 or 55AA )

 ; 设置磁盘支持扩展中断标志
 MOV  BYTE [BP - DISK_EXT_SUPPORT],01H  ; 01H表示支持磁盘扩展

; 不支持磁盘扩展
_NO_DISK_EXTENSION:

 ; 检查是否为FAT32分区
 ; 对于FAT32分区，其根目录项为0
 CMP  WORD [RootEntries],0
 JNZ  NEAR _DISK_ERROR

 ; 检查保留扇区数(保留扇区必须大于等于4，我们将
 ; 第二个扇区的代码存在第四个扇区)
 CMP  WORD [ReservedSectors],4
 JB  NEAR _DISK_ERROR

 ; 检查每FAT扇区数
 ; SectorsPerFAT16 == 0
 ; SectorsPerFAT32 != 0
 CMP  WORD [SectorsPerFAT16],0
 JNZ  NEAR _DISK_ERROR
 CMP  DWORD[SectorsPerFAT32],0
 JZ  NEAR _DISK_ERROR

 ; 计算每个扇区包含的目录项 ( 512/32 = 16 = 10H )
 MOV  AX,WORD [BytesPerSector]
 MOV  CL,DIR_ENTRY_SIZE
 DIV  CL        ; AH:AL = BytesPerSector / 32 ( AH = Remainder = 0 )
 MOV  BYTE [BP - DIR_PER_SECTOR],AL ; AL    = DirEntriesPerSector

 ; FAT起始扇区
 ; FAT起始扇区 = Hidden+Reserved
 MOV  AX ,WORD [ReservedSectors]
 CWD          ; AX => DX : AX
 ADD  AX, WORD [HiddenSectors]
 ADC   DX, WORD [HiddenSectors+2]
 MOV  WORD[ BP - FAT_START_SECTOR  ],AX
 MOV   WORD[ BP - FAT_START_SECTOR+2],DX


 ; FAT表所占的扇区数
 ; FAT_SECTORS = NumberOfFAT * SectorsPerFAT
 XOR  EAX,EAX
 MOV  AL, BYTE [NumberOfFATs]  ; FAT的数目
 MOV  EBX,DWORD [SectorsPerFAT32]
 MUL  EBX        ; 乘积放入 EDX:EAX
 MOV  DWORD [ BP - FAT_ENTRY_SECTORS  ] , EAX

 ; 计算数据区起始扇区
 ADD  EAX ,DWORD[ BP - FAT_START_SECTOR  ]
 MOV  DWORD [ BP - DATA_START_SECTOR ],EAX


 ;====================================================================
 ;
 ; 初始化DiskAddressPacket
 ; 使用时只需要修改字段：DATA_BUFFER_OFF DATA_BUFFER_SEG
 ;       DAP_SECTOR_LOW  DAP_SECTOR_HIGH
 ;
 ;====================================================================
 MOV  DWORD [BP - DAP_SECTOR_HIGH ],00H
 MOV  BYTE  [BP - DAP_RESERVED1   ],00H
 MOV  BYTE  [BP - DAP_RESERVED2   ],00H
 MOV  BYTE  [BP - DAP_PACKET_SIZE ],10H
 MOV  BYTE  [BP - DAP_READ_SECTORS],01H
 MOV  WORD  [BP - DAP_BUFFER_SEG  ],00H
 MOV  BYTE  [BP - DAP_READ_SECTORS],01H  ; 每次只读取一个扇区

 ; 装载第二个启动扇区
 MOV  WORD  [BP - DAP_BUFFER_OFF  ],SECOND_ADDR
 MOV  EAX , DWORD[HiddenSectors]
 ADD  EAX , SECOND_SECTOR
 MOV  DWORD [BP - DAP_SECTOR_LOW  ],EAX
 CALL ReadSector


 ; 下面开始查找根目录并且装载FDOSLDR.BIN
 JMP  _SEARCH_LOADER

;====================================================================
; 错误处理
;====================================================================
_MISSING_LOADER:     ; 显示没有装载程序
 MOV  SI,MessageMissLoader
 CALL ShowMessage
 JMP  _REBOOT

_DISK_ERROR:      ; 显示磁盘错误信息
 MOV  SI,MessageDiskError
 CALL ShowMessage

_REBOOT:       ; 重启
 MOV  SI,MessageRestart
 CALL ShowMessage

 ; 调用键盘中断，等待用户按键
 MOV  AH,00H
 INT  16H

 ; 重启计算机
 INT  19H

 ; 死循环
 JMP  $


;====================================================================
;
; 子过程
;
;====================================================================

;====================================================================
;
; 读取一个磁盘扇区
; 输入： 已经设置了DAP中相应的字段
; 限制： 不能读取超过一个簇的内容
;
;====================================================================
ReadSector:

 PUSHA  ; 保存寄存器

 ; 检查是否使用扩展方式
 CMP  BYTE [BP - DISK_EXT_SUPPORT],00H
 JZ  .NoDiskExtension

;====================================================================
; INT 13H  AH = 42H 扩展磁盘调用
;====================================================================
 ; 每次读取一个扇区
 MOV  AH,42H         ; 功能号
 LEA  SI ,[BP - DAP_PACKET_SIZE]    ; 地址包地址

 ; 驱动器号
 MOV  DL ,[DriveNumber]      ; 驱动器号
 INT  13H
 JC   _DISK_ERROR        ; 读取失败
 JMP  _READ_SECTOR_OK       ; 读取成功

;====================================================================
;
; INT 13H
;  AH = 2        柱面号：0 - 1023
;  AL = 要读取的扇区数     磁头号：0 - 255
;  CH = 柱面号低8位     扇区号：1 - 63
;  CL = 柱面号高2位 : 6位扇区号
;  DH = 磁头号
;  DL = 驱动器号
;
; LBA = ( (cylinder * HeadsPerCylinder + heads ) * SectorsPerTrack ) + sector - 1
;
; Sector = LBA % SectorsPerTrack +1
; Head = (  LBA / SectorsPerTrack ) % HeadsPerCylinder
;   Cylinder= (  LBA / SectorsPerTrack ) / HeadsPerCylinder
;
;====================================================================
.NoDiskExtension:

 ;===================================================================
 ; 首先需要将扇区号转换为CHS地址
 ;===================================================================

 ; 首先计算扇区号
 MOV  AX,WORD [ BP - DAP_SECTOR_LOW   ]
 MOV  DX,WORD [ BP - DAP_SECTOR_LOW+2 ]
 DIV  WORD [SectorsPerTrack ] ; AX = LBA / SectorsPerTrack DX = LDA % SectorsPerTrack
 MOV  CX,DX
 INC  CX      ; CL = Sector
 AND  CL,3FH     ; 1-63

 ; 再计算磁头号和柱面号
 XOR  DX,DX     ; DX:AX = LBA / SectorsPerTrack
 DIV  WORD [HeadsPerCylinder] ; DX = ( LBA/SectorsPerTrack ) %  HeadsPerCylinder = Head
         ; AX = ( LBA/SectorsPerTrack ) /  HeadsPerCylinder = Cylinder
 MOV  CH,AL     ; 柱面号低8位
 SHL  AH,6
 OR  CL,AH     ; CL = 柱面号高2位：6位扇区号
 MOV  DH,DL     ; DL = 磁头号

 ; 准备读取磁盘
 MOV  AX,WORD[ BP - DAP_BUFFER_SEG ]
 MOV  ES,AX
 MOV  BX,WORD[ BP - DAP_BUFFER_OFF ]
 MOV  AX ,0201H    ; 每次只读取一个扇区

 ; 驱动器号
 MOV  DL ,[DriveNumber]      ; 驱动器号
 INT  13H
 JC   _DISK_ERROR        ; 读取失败

_READ_SECTOR_OK:
 POPA       ; 恢复寄存器
 RET

;====================================================================
;
; 显示一个字符串
; 输入：
;   DS:SI  = 字符串的起始地址(以NULL结束)
;
;====================================================================
ShowMessage:
 LODSB    ; AL = DS:[SI] SI = SI+1
 OR  AL,AL    ; 检测是否遇到NULL字符串
 JZ  _SHOW_END
 MOV  AH,0EH
 MOV  BX,07H
 INT  10H
 JMP  ShowMessage

_SHOW_END:
 RET

;====================================================================
; 数据区
;====================================================================
LoaderName     db "FDOSLDR BIN"       ; 第二阶段启动程序 FDOSLDR.BIN
MessageMissLoader   db "NO FDOSLDR.BIN.",0DH,0AH,00H   ; 没有找到装载程序
MessageDiskError   db  "Disk Error.",0DH,0AH,00    ; 磁盘错误消息
MessageRestart    db "Press any key to restart." ,0DH,0AH,00 ; 提示重启消息
;====================================================================
; 扇区最后的标记字节(NASM不支持重复ORG)
;====================================================================
Padding TIMES 510-($-$$) db  00H
SectorSignature    dw 0AA55H



;====================================================================
; 第二个扇区的代码(该代码位于分区的第四个扇区)
;====================================================================

;====================================================================
; 查找根目录，检查是否有 FDOSLDR.BIN文件
;====================================================================
_SEARCH_LOADER:


 ; 设置缓冲区
 MOV  WORD [ BP - DAP_BUFFER_OFF  ], DATA_BUF_OFF ; 0000:1000H

 ; 根目录起始簇号
 MOV  EAX,DWORD[RootDirectoryStart]
 MOV  DWORD[ BP - CURRENT_CLUSTER ],EAX

; 检查下一个簇
_NEXT_ROOT_CLUSTER:

 ; 根据簇号计算扇区号(EAX-2)*SectorsPerCluster+DATA_START_SECTOR
 DEC  EAX
 DEC  EAX  ; EAX = EAX - 2
 XOR  EBX,EBX
 MOV  BL, BYTE[ SectorsPerCluster]
 MUL  EBX
 ADD  EAX,DWORD[ BP- DATA_START_SECTOR]
 MOV  DWORD[ BP - DAP_SECTOR_LOW  ], EAX
 MOV  DL,[SectorsPerCluster]

; 检查下一个扇区
_NEXT_ROOT_SECTOR:

 ; 依次读取每个根目录扇区，检查是否存在FDOSLDR.BIN文件
 CALL ReadSector

 ; 检查该扇区内容
 MOV  DI,DATA_BUF_OFF
 MOV  BL,BYTE [ BP - DIR_PER_SECTOR]

; 检查每一个目录项
_NEXT_ROOT_ENTRY:
 CMP  BYTE [DI],DIR_NAME_FREE
 JZ  _MISSING_LOADER    ; NO MORE DIR ENTRY

 ; 检查是否装载程序
 PUSH  DI       ; 保存DI
 MOV  SI,LoaderName
 MOV  CX,11
 REPE  CMPSB
 JCXZ  _FOUND_LOADER    ; 装载Loader并运行

 ; 是否还有下一个目录项(内层循环)
 POP  DI
 ADD   DI,DIR_ENTRY_SIZE
 DEC  BL
 JNZ   _NEXT_ROOT_ENTRY

 ; 检查是否还有下一个扇区可读(外层循环)
 DEC  DL
 JZ  _CHECK_NEXT_ROOT_CLUSTER
 INC  DWORD [ BP - DAP_SECTOR_LOW ] ; 增加扇区号
 JMP  _NEXT_ROOT_SECTOR

; 检查下一个簇
_CHECK_NEXT_ROOT_CLUSTER:

 ; 计算FAT所在的簇号和偏移
 ; FatOffset = ClusterNum*4
 XOR  EDX,EDX
 MOV  EAX,DWORD[BP - CURRENT_CLUSTER]
 SHL  EAX,2
 XOR  ECX,ECX
 MOV  CX,WORD [ BytesPerSector ]
 DIV  ECX  ; EAX = Sector EDX = OFFSET
 ADD  EAX,DWORD [BP - FAT_START_SECTOR  ]
 MOV  DWORD [ BP - DAP_SECTOR_LOW ], EAX

 ; 读取扇区
 CALL  ReadSector

 ; 检查下一个簇
 MOV  DI,DX
 ADD  DI,DATA_BUF_OFF
 MOV  EAX,DWORD[DI]  ; EAX = 下一个要读的簇号
 AND  EAX,CLUSTER_MASK
 MOV  DWORD[ BP - CURRENT_CLUSTER ],EAX
 CMP  EAX,CLUSTER_LAST  ; CX >= 0FFFFFF8H，则意味着没有更多的簇了
 JB  _NEXT_ROOT_CLUSTER
 JMP  _MISSING_LOADER

;====================================================================
; 装载FDOSLDR.BIN文件
;====================================================================
_FOUND_LOADER:
 ; 目录结构地址放在DI中
 POP  DI
 XOR  EAX,EAX
 MOV  AX,[DI+OFF_START_CLUSTER_HIGH] ; 起始簇号高32位
 SHL  AX,16
 MOV  AX,[DI+OFF_START_CLUSTER_LOW]  ; 起始簇号低32位
 MOV  DWORD[ BP - CURRENT_CLUSTER ],EAX
 MOV  CX, OSLOADER_SEG      ; CX  = 缓冲区段地址


_NEXT_DATA_CLUSTER:

 ; 根据簇号计算扇区号
 DEC  EAX
 DEC  EAX  ; EAX = EAX - 2
 XOR  EBX,EBX
 MOV  BL, BYTE[ SectorsPerCluster]
 MUL  EBX
 ADD  EAX,DWORD[ BP- DATA_START_SECTOR]
 MOV  DWORD[ BP - DAP_SECTOR_LOW  ], EAX
 MOV  DL,[SectorsPerCluster]

 ; 设置缓冲区
 MOV  WORD [ BP - DAP_BUFFER_SEG   ],CX
 MOV  WORD [ BP - DAP_BUFFER_OFF   ],00H

 ; 每个簇需要读取的扇区数
 MOV  BL , BYTE [SectorsPerCluster]

_NEXT_DATA_SECTOR:
 ; 读取簇中的每个扇区(内层循环)
 ; 注意 : 通过检查文件大小，可以避免读取最后一个不满簇的所有大小
 ; 读取数据扇区
 CALL  ReadSector

 ; 更新地址，继续读取
 MOV  AX, WORD [BytesPerSector]
 ADD  WORD  [BP - DAP_BUFFER_OFF],AX
 INC  DWORD [BP - DAP_SECTOR_LOW]  ; 递增扇区号
 DEC  BL        ; 内层循环计数
 JNZ  _NEXT_DATA_SECTOR


 ; 检查下一个簇

 ; 更新读取下一个簇的缓冲区地址
 MOV  CL,BYTE [ SectorsPerCluster ]
 MOV  AX ,WORD [BytesPerSector]
 SHR  AX ,4
 MUL  CL
 ADD  AX ,WORD [ BP - DAP_BUFFER_SEG ]
 MOV  CX,AX ; 保存下一个簇的缓冲区段地址

 ;====================================================================
 ;
 ; 检查是否还有下一个簇(读取FAT表的相关信息)
 ;  LET   N = 数据簇号
 ;  THUS FAT_BYTES  = N*4  (FAT32)
 ;      FAT_SECTOR = FAT_BYTES / BytesPerSector
 ;    FAT_OFFSET = FAT_BYTES % BytesPerSector
 ;
 ;====================================================================

 ; 计算FAT所在的簇号和偏移
 MOV  EAX,DWORD [BP - CURRENT_CLUSTER]
 XOR  EDX,EDX
 SHL  EAX,2
 XOR  EBX,EBX
 MOV  BX,WORD [ BytesPerSector ]
 DIV  EBX   ; EAX = Sector  EDX = Offset

 ; 设置缓冲区地址
 ADD  EAX,DWORD [BP - FAT_START_SECTOR  ]
 MOV  DWORD [ BP - DAP_SECTOR_LOW ], EAX
 MOV   WORD [BP - DAP_BUFFER_SEG  ], 00H
 MOV  WORD [BP - DAP_BUFFER_OFF  ], DATA_BUF_OFF ; 0000:1000H

 ; 读取扇区
 CALL  ReadSector

 ; 检查下一个簇
 MOV  DI,DX
 ADD  DI,DATA_BUF_OFF
 MOV  EAX,DWORD[DI]  ; EAX = 下一个要读的簇号
 AND  EAX,CLUSTER_MASK
 MOV  DWORD[ BP - CURRENT_CLUSTER ],EAX
 CMP  EAX,CLUSTER_LAST  ; CX >= 0FFFFFF8H，则意味着没有更多的簇了
 JB  _NEXT_DATA_CLUSTER

;读取完毕
_RUN_LOADER:

 ; 运行FDOSLDR.BIN
 MOV  DL,[DriveNumber]
 jmp 800h:0100h;JMP  00:OSLOADER_ADDR

;====================================================================
; 调试例程
;====================================================================
%IFDEF DEBUG
;====================================================================

;====================================================================
;
; 显示一个字符
; 输入： AL = 待显示字符
;
;====================================================================
PrintChar:
 PUSH BX
 MOV  AH,0EH
 MOV  BX,7
 INT  10H
 POP  BX
 RET

;====================================================================
%ENDIF ; DEBUG
;====================================================================


;====================================================================
; 扇区最后的标记字节(NASM不支持重复ORG)
;====================================================================
SecondPadding  TIMES 1022-($-$$) db  00H
SecondSignature  DW 0AA55H

;====================================================================
; 代码结束
;====================================================================

