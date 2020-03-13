org 0x7c00
BaseOfStack equ 0x7c00
BaseOfLoader equ 0x1000
OffsetOfLoader equ 0x00
RootDirSectors equ 14
SectorNumOfRootDirStart equ 19
SectorNumOfFAT1Start equ 1
SectorBalance equ 17
jmp short Label_Start
nop
BS_OEMName db 'MINEboot';OEM字符串
BPB_BytesPerSec dw 512;每扇区字节数
BPB_SecPerClus db 1;每簇占用的扇区数
BPB_RsvdSecCnt dw 1;Boot占用的扇区数
BPB_NumFATs db 2;FAT表的记录数
BPB_RootEntCnt dw 224;最大根目录文件数
BPB_TotSec16 dw 2880;逻辑扇区总数
BPB_Media db 0xf0;媒体描述符
BPB_FATSz16 dw 9;每个FAT占用扇区数
BPB_SecPerTrk dw 18;每个磁道扇区数
BPB_NumHeads dw 2;磁头数
BPB_HiddSec dd 0;隐藏扇区数
BPB_TotSec32 dd 0;如果BPB_TotSec16 是 0,则在这里记录扇区总数
BS_DrvNum db 0;中断 13 的驱动器号
BS_Reserved1 db 0;未使用
BS_BootSig db 29h;扩展引导标志
BS_VolID dd 0 ;卷序列号
BS_VolLab db 'boot loader';卷标必须11个字符 不足以空格填充
BS_FileSysType db 'FAT12   ';文件系统类型,必须是8个字符,不足填充空格
;=============== 读取一个扇区 从软盘
Func_ReadOneSector:
    push bp
    mov bp,sp
    sub esp,2
    mov byte [bp - 2],cl
    push bx
    mov bl,[BPB_SecPerTrk]
    div bl
    inc ah
    mov cl,ah
    mov dh,al
    shr al,1
    mov ch,al
    and dh,1
    pop bx
    mov dl,[BS_DrvNum]
Label_Go_On_Reading:
	mov ah,2;ah = 02 功能号
	mov al,byte [bp -2];读入的扇区数
	int 13h;调用中断 02 的功能读取磁盘扇区
	jc Label_Go_On_Reading;读取成功
	add esp,2;恢复栈 调用前状态
	pop bp;恢复bp
	ret	;返回调用
mov word [SectorNo],SectorNumOfRootDirStart;起始搜索扇区
Lable_Search_In_Root_Dir_Begin:
	;		翻译成C代码:
	;	buff 指向写出的缓冲地址
	;	SectorNo = 0 从0扇区开始搜索
	; if RootDirSizeForLoop == 0
	;{
	;	Label_No_LoaderBin()//输出没找到 文件
	;	}
	;for i:=RootDirSizeForLoop;i>0;i--{
	;	SectorNo ++ 
	;	Func_ReadOneSector(SectorNo,1,&buff)
	;}
	cmp word [RootDirSizeForLoop],0 ; 如果 根目录数 为0
	jz Label_No_LoaderBin ;
	dec word [RootDirSizeForLoop];要搜索的根目录数 -1
	mov ax,00h;设置 ax 寄存器
	mov es,ax;设置段基址寄存器
	mov bx,8000h;读出 es:bx 数据段缓冲区
	mov ax,[SectorNo];参数 :要读取的LBA扇区号
	mov cl,1;参数 :读取1个扇区
	call Func_ReadOneSector;调用读取 一个扇区方法
	mov si,LoaderFileName;读取文件名 数据段寄存器 ds:si 获得
	mov di,8000h;将文件名 写到 es:di 附加段 由于Func_ReadOneSector 将数据 写出到了 es:bx 0000:8000h 这里的内存 所以后面用 es:di 来读取这块的内存
	cld;设置DF标志位 为 0 
	mov dx,10h;循环遍历 每个扇区的目录数 一个扇区 512 字节/ 目录占32 字节 = 10h
Label_Search_For_LoaderBin:
	cmp dx,0;循环 遍历 一个扇区的数量
	;		翻译成C代码:
	;for i = 每个扇区目录数;i > 0 ;i--{
	;		Label_Cmp_FileName(loaderfilename,cmpfilename)//比较 每个目录的名字
	;	}
	;goto Label_Goto_Next_Sector_In_Root_Dir
	jz Label_Goto_Next_Sector_In_Root_Dir
	dec dx
	mov cx,11;文件名 占 11个字节 所以循环 11次 逐个比较 字节
Label_Cmp_FileName:
	;逐字节 比较文件名
	;var loaderfilename = "LOADER BIN"
	;var cmpfilename = "xxxxxxx"
	; for i:=0;i<len(loaderfilename);i++{
	;	if loaderfilename[i] != cmpfilename[i]{
	;		goto Label_Different
	;	}
	;}
	; Label_FileName_Found () //找到文件了
	cmp cx,0
	jz Label_FileName_Found
	dec cx
	lodsb;LODSB将一个字节装入al 源地址为DS:SI
	cmp al,byte [es:di];判断 文件字符串
	jz Label_Go_On
	jmp Label_Different;不同跳出循环
Label_Go_On:
	inc di;偏移 -1
	jmp Label_Cmp_FileName;继续循环比较字节
Label_Different:
	;目录名没有找到 
	and di,0ffe0h;这里 后 5位清零 取 32 的 倍数 清除上面对 对di的作用  如果用  and di,0ff00h 也是一样的
	add di,20h;每个目录相隔 32 位 定位到 下个目录 的文件名
	mov si,LoaderFileName ;将 ds:si 再次指向 文件名开始位置。
	jmp Label_Search_For_LoaderBin
Label_Goto_Next_Sector_In_Root_Dir:
	;没找到的话 就到下一个扇区取寻找
	add word [SectorNo],1;扇区号 + 1
	jmp Lable_Search_In_Root_Dir_Begin;递归调用
Label_No_LoaderBin:
	;向屏幕输出 没有找到 引导文件
	mov ax,1301h
	mov bx,008ch
	mov dx,0100h
	mov cx,21
	push ax
	mov ax,ds
	mov es,ax
	pop ax
	mov bp,NoLoaderMessage
	int 10h
	jmp $
;输入参数 AH = FAT表项号 根据表项号 索引出下一个表项
Func_GetFATEntry:
	push es;保存附加段寄存器
	push bx;保存 bx寄存器
	push ax;保存 ax
	mov ax,00
	mov es,ax;设置es 附加段寄存器 为 00
	pop ax;恢复ax
	mov byte [Odd],0;设置 Odd 为 0 
	mov bx,3;设置 bx 为 3
	mul bx;ax 乘以3 
	mov bx,2
	div bx;除以2 此时 ax 为这个 表项所在开始字节
	cmp dx,0 ;判断 是否有 余数 如果有余数 Odd 置为 1
	jz Label_Even;如果 能整除 Odd 此时为 0 
	mov byte [Odd],1;如果不能整除 Odd 为 1
Label_Even:
	xor dx,dx;清零
	mov bx,[BPB_BytesPerSec];字节每个扇区
	div bx;ax / bx 商 为FAT表项的偏移扇区号,余数值为FAT表项在扇区中的偏移位置
	push dx;dx 表项所在字节
	mov bx,8000h;参数 读取到文件内容地址
	add ax,SectorNumOfFAT1Start;表项所在扇区: 表项起始扇区 + 表项扇区偏移
	mov cl,2;读取2个 扇区是因为有时需要跨扇区读取文件。这时由于 512 /1.5 不能整除 如果换成 FAT16 就没这个问题了。
	call Func_ReadOneSector;读 表项数据
	pop dx;恢复dx
	add bx,dx;dx 为 读取扇区数据所在位置
	mov ax,[es:bx];
	cmp byte [Odd],1;如 是 不能被整数的项 需要单独处理
	jnz Label_Even_2;如果 能被整除 跳到 Label_Even_2
	shr ax,4;右移4位  请看上图
Label_Even_2:
	and ax,0fffh;取 后面12位
	pop bx;恢复bx
	pop es;恢复es
	ret	;返回
Label_FileName_Found:
	mov ax,RootDirSectors;ax = 根目录 占用扇区数
	and di,ffe0h;取 32 的倍数 保证 指向 某一个目录 
	add di,1ah;加 26 个字节 指向 文件簇号 
	mov cx,word [es:di];取出簇号
	push cx;压入簇号
	add cx,ax;根目录扇区数 加 簇号
	add cx,SectorBalanece; 加上 这个 可以定位到 数据区的表项 注意 数据区表项起始位置要 减去 2
	mov ax,BaseOfLoader;段地址
	mov es,ax;设附加段置段地址
	mov bx,OffsetOfLoader;要跳转的 loader 偏移地址
	mov ax,cx;ax 指向数据区
Label_Go_On_Loading_File:
	push ax;保存 ax
	push bx;保存 bx
	mov ah,0eh
	mov al,'.'
	mov bl,0fh
	int 10h ;输出 .
	pop bx
	pop ax
	mov cl,1;读取扇区数
	call Func_ReadOneSector ;ax 指向数据区  将数据 读到 BaseOfLoader 内存处
	pop ax;恢复ax
	call Func_GetFATEntry ;获取下一个 表项
	cmp ax,0fffh;如果 表项的12 位 读到了 0fffh 就认为是最后一个扇区了
	jz Label_File_Loaded;跳转到 加载到的内存储
	push ax;指向 数据区的位置
	mov ax,RootDirSectors ;ax = 目录扇区
	add ax,dx;目录对应位置
	add ax,SectorBalance;扇区数
	add bx,[BPB_BytesPerSec]
	;循环调用 读取扇区
	jmp Label_Go_On_Loading_File
Label_File_Loaded:
	;跳转到 loader程序 处
	jmp BaseOfLoader:OffsetOfLoader	;局部变量
RootDirSizeForLoop dw RootDirSectors
SectorNo dw 0
Odd db 0
;显示消息
StartBootMessage db "Start Boot";开始读取 提示
NoLoaderMessage: db "ERROR:NO LOADER FOUND";没找到文件提示
LoaderFileName: db "LOADER BIN",0;要读取文件名
