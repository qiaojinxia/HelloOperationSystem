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



## BOCHS常用调试指令

- `s` : 单步执行
- `b` : 断点设置
- `c` : 继续执行
- `r` : 查看寄存器
- `sreg` : 查看段寄存器
- `xp` ： 查看内存 xp /[num]bx [seg]:[offset]





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

#### CPU 分页机制

<img src="/Users/qiao/Library/Application Support/typora-user-images/image-20200322163637922.png" alt="image-20200322163637922" style="zoom:50%;" />

页内部连续的线性地址映射到连续的物理地址中

32位 下 分页大小为 4Kb.CPU分页 要借助 页表,页表 存放在内存中

保护模式下,控制寄存器CR0的最高位PG位控制分页的开启 PG =1 此时通过页表才能把线性地址转换为物理地址,

PG = 0 分页机制无效,线性地址就直接作为 物理地址。

每个任务可以有自己的页目录表和页表

#####  为了节约内,8086 CPU 的设计者将线性地址通过页目录表和页表两级查找转换成物理地址。

1. 32模式下线性地址通过3个部分组成：

   - 最高10位 Directory 页目录表偏移量

     - 页目录表的大小为4KB(32位模式下),记录一个地址需要4K在32位模式下,如果 内存被拆成 4KB 为最小单位,那么 4G = 2 的 20次方 * 4KB = $2^{20} * 1024 * 4$  如果页表是一个大数组,每个页数组记录连续4KB的起始位置,那么 每个地址32位地址栈4个字节  需要$2^{20} * 4$  = 4M的大小存放,Intel觉得这样太耗费空间了,将其拆分成 3个索引 通过计算编程一个线性地址。

       下面来个直观的例子:

       <img src="/Users/qiao/Library/Application Support/typora-user-images/image-20200322163417627.png" alt="image-20200322163417627" style="zoom:50%;" />

   - 中间10位 Table 页表偏移量

   - 最后12位Ofsset是屋里也内侧字节偏移量。

   

   

   

   进程

   

   



