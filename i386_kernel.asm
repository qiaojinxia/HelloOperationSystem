
extern dis_position

global low_print
;============================
;             打印字符                   
;   函数原型 : void low_print(char* str),以0结尾                       
;=============================
low_print:
  push  esi
  push  edi
  mov   esi,[esp + 0xc] ;指向栈向前第六个指针 字符串指针
  mov   edi,[dis_position] ;输出起始位置
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
  mov dword [dis_position], edi ; 打印完毕，更新显示位置
  pop   edi
  pop   esi
  ret