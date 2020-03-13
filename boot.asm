org 0x7c00;程序加载到的内存
BaseOfStack equ 0x7c00
Label_Start:
mov ax,cs;取代码段寄存器0x7c00
mov ds,ax ;设置数据段寄存器
mov es,ax ;设置附加寄存器
mov ss,ax ;设置栈顶
mov sp,BaseOfStack ;设置栈底
;清屏 相当于窗口的滚动条 滚动了指定行 实现清屏
mov ax,0600h ;按指定范围滚动窗口
mov bx,0700h;设置显示的属性
mov cx,0;滚动范围的左上角坐标行列号
mov dx,184fh;.....右下角行列号
int 0x10
;设置光标位置
mov ax,0200h ;AH02 功能:设置屏幕光标位置
mov bx,0000h ;BH页码
mov dx,0000h ;dx 游标的列行数
int 0x10 ;调用硬件中断 实现光标设置
mov ax,1301h;AH = 13h显示一行字符 AL = 01h 显示字符串光标移动到字符串末尾
mov bx,000fh; BH:页码 BL:字符属性/颜色属性
mov dx,0000h;游标的坐标行列号
mov cx,10;字符串的长度
push ax ;保存ax
mov ax,ds ;得到数据段地址
mov es,ax ;附加段 设置为 数据段
pop ax ;恢复ax
mov bp,StartBootMessage ;ES:BP 段偏移 << 4 + bp 放的是要显示的字符
int 0x10
jmp $ ;让系统停在这 死循环
StartBootMessage db "Start Boot"
times 510 -($ - $$) db 0 ;计算前面代码用了多少字节 填充00到剩余 直到510字节
dw 0xaa55 ;识别引导扇区标识


