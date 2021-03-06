; LOADER.BIN 被加载的位置　--- 段地址
LOADER_SEG        equ     0x9000
; LOADER.BIN 被加载的位置　--- 偏移地址
LOADER_OFFSET      equ     0x100
; LOADER.BIN 被加载到的位置　--- 物理地址　(= LOADER_SEG * 10h)
LOADER_PHY_ADDR     equ LOADER_SEG * 10h


; KERNEL.BIN 被加载到的位置 ----  段地址
KERNEL_SEG	        equ	    0x7000
; KERNEL.BIN 被加载到的位置 ---- 偏移地址
KERNEL_OFFSET	equ	    0
; KERNEL.BIN 被加载到的位置　--- 物理地址　(= KERNEL_SEG * 10h)
KERNEL_PHY_ADDR	equ	KERNEL_SEG * 10h

SearchFileSector	equ	0xff;搜索多少个扇区目录

DirSize	equ	0x008    ;扇区字节数/目录项占用大小
OffsetOfLoader	equ	0x00    ;
BaseOfStack         equ 0x100                                   ; 基栈
FatPerSector	equ	0x0080	

KERNEL_ENTRY_POINT_PHY_ADDR     equ 0x1000  ; ld -m elf_i386 -Ttext 0x1000 -o kernel.bin kernel.o main.o i386_kernel.o 通过 -Ttext 指定入口点的地址

;页目录的其实位置 1M处
PDE_ORIGIN_ADDRESS  equ 0x100000

;页表其实位置 1M往上4K处开始
PTE_ORIGIN_ADDRESS equ 0x101000