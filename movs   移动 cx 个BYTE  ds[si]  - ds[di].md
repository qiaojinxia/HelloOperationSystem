## 操作系统小知识



### Nasm相关知识

org xxx 和  vstart = xxx 是一样的功能

它告诉编译器 你的 segment 开始的位置,

如果指定了 vStart = 0 那么 $ 就会从 这个段 开始计算到当前行偏移多少个字节

如果指定了 vStart = xxx  $$ 获取获取的是就是 xxx

所以 org 和 vstart 只是编译器在 用于计算 代码位置的 并不是 意味着 代码会 放在xxx地址。

> start:
>
> ​		mov ax,start

如果不指定 org 那么 反编译后 会是 mov ax,0。

如果 org = 0x700

那么编译器就会计算 成 段开始 + 代码偏移地址,

然后替换成 mov ax,0x7c00 + 0x00

### 常用的一些汇编指令:

movs   移动 cx 个BYTE  ds:[si]  -> ds:[di]







## 实模式下知识点





常用寄存器:

![image-20200319163749198](/Users/qiao/Desktop/oscode/常用寄存器说明.png)



### 实模式下 CPU 寻址方式



1. 寄存器寻主 

2. 立即数寻址

3. 内存寻址

   - 直接寻址
   - 机制寻址
   - 变址寻址
   - 基址变址寻址

   <img src="/Users/qiao/Library/Application Support/typora-user-images/image-20200319171309628.png" alt="image-20200319171309628" style="zoom:50%;" />

