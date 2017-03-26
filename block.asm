data segment
    ;--------------------------------开始界面------------------------------------------------------
    menu00	db           "************************************************************$"
    menu01	db 0ah, 0dh, "*  Copyright@2016.12                                       *$"
    menu02	db 0ah, 0dh, "*                                           @        @     *$"
    menu03	db 0ah, 0dh, "*                                         @   @    @   @   *$"
    menu04	db 0ah, 0dh, "*                    Magical   Block                       *$"
    menu05	db 0ah, 0dh, "*                                               @          *$"
    menu06	db 0ah, 0dh, "*                                                          *$"
    menu07	db 0ah, 0dh, "*                                           @         @    *$"
    menu08	db 0ah, 0dh, "*            1.Play                          @       @     *$"
    menu09	db 0ah, 0dh, "*                                              @ @ @       *$"
    menu10	db 0ah, 0dh, "*            2.Exit                                        *$"
    menu11	db 0ah, 0dh, "*                                                          *$"
    menu12	db 0ah, 0dh, "*              Welcome !!! let's play (1/2)                *$"
    menu13   db 0ah, 0dh,"************************************************************$"

    ;------------------------------游戏界面-----------------------------------------------------------
    figure00 db           "************************************************************$"
    figure01 db 0ah, 0dh, "*                                      *                   *$"
    figure02 db 0ah, 0dh, "*                                      *                   *$"
    figure03 db 0ah, 0dh, "*                                      *                   *$"
    figure04 db 0ah, 0dh, "*                                      *                   *$"
    figure05 db 0ah, 0dh, "*                                      *                   *$"
    figure06 db 0ah, 0dh, "*******                                *********************$"
    figure07 db 0ah, 0dh, "*     *                                * best score:       *$"
    figure08 db 0ah, 0dh, "* M   *                                *                   *$"
    figure09 db 0ah, 0dh, "* A   *                                * Time              *$"
    figure10 db 0ah, 0dh, "* G  R*                                *                   *$"
    figure11 db 0ah, 0dh, "* I  A*                                * Score:            *$"
    figure12 db 0ah, 0dh, "* C  I*                                *********************$"
    figure13 db 0ah, 0dh, "*    N*                                * action:           *$"
    figure14 db 0ah, 0dh, "* B  B*                                *     left    <-    *$"
    figure15 db 0ah, 0dh, "* L  O*                                *     up      |^|   *$"
    figure16 db 0ah, 0dh, "* O  W*                                *     down    |V|   *$"
    figure17 db 0ah, 0dh, "* C   *                                *     right   ->    *$"
    figure18 db 0ah, 0dh, "* K   *                                * sys:              *$"
    figure19 db 0ah, 0dh, "* !   *                                *   start   enter   *$"
    figure20 db 0ah, 0dh, "* !   *                                *   quit    q       *$"
    figure21 db 0ah, 0dh, "************************************************************$"

;-----------------------------------------------------------------------------------------------

	g_over01 db           "************************************************************$"
	g_over02 db 0ah, 0dh, "*                                                          *$"
	g_over03 db 0ah, 0dh, "*        * **        *       **      **   ** ** * *        *$"
    g_over04 db 0ah, 0dh, "*      *            * *      * *    * *   *                *$"
    g_over05 db 0ah, 0dh, "*      *   ***     *   *     *  *  *  *   ** * * *         *$"
    g_over06 db 0ah, 0dh, "*      *     *    * *** *    *   *    *   *                *$"
    g_over07 db 0ah, 0dh, "*        * *     *       *   *        *   ** ** * *        *$"
    g_over08 db 0ah, 0dh, "*                                                          *$"
    g_over09 db 0ah, 0dh, "*                                                          *$"
    g_over10 db 0ah, 0dh, "*          * * *     *          * ** ** * *   * * *  *     *$"
    g_over11 db 0ah, 0dh, "*       *        *    *        *  *           *       *    *$"
    g_over12 db 0ah, 0dh, "*      *          *    *      *   ** * * *    *       *    *$"
    g_over13 db 0ah, 0dh, "*      *          *     *    *    *           *  *  *      *$"
    g_over14 db 0ah, 0dh, "*       *        *       *  *     *           *    *       *$"
    g_over15 db 0ah, 0dh, "*          * * *          **      ** ** * *   *     * *    *$"
    g_over16 db 0ah, 0dh, "*                                                          *$"
    g_over17 db 0ah, 0dh, "*                                                          *$"
	g_over18 db 0ah, 0dh, "*                                                          *$"
	g_over19 db 0ah, 0dh, "************************************************************$"
	g_over20 db 0ah, 0dh, "*   Powered by ZhangMingRui  DuLun  YangLiRu  [2016.12]    *$"
	g_over21 db 0ah, 0dh, "************************************************************$"

;-------------------------------------------------------------------------------------------

    now_block       db 1,40;当前木块位置
    ;start_block     db 1,40;

    square_block    db 4,0,0,0,1,1,0,1,1,0;正方形
    last_choice     db 0
    line_block      db 4,0,0,0,1,0,2,0,3,1;横条
    line_block2     db 4,0,1,1,1,2,1,3,1,2;竖条

    L_block         db 4,0,0,1,0,1,1,1,2,3;L形
    L_block2        db 4,0,2,1,0,1,1,1,2,4;

    T_block         db 4,0,1,1,0,1,1,2,1,5;T形
    T_block2        db 4,0,1,1,2,1,1,2,1,6

    Z_block         db 4,0,0,0,1,1,2,1,0,7;Z形
    Z_block2        db 4,0,0,1,0,1,1,2,1,8



    move_index      db 0,0
    choice_block    db 0;当前木块
    what_block      db 0

    approve         db 0;当前位置
    first_move      db 0

    now_line        db 0

    who_block       db 0
    block_colar     db 0
    score           db 0
    small_block		db 0;右上角木块

    path1           db 'c:/score.txt'
    buffer1         db 0
    buffer2         db 49
data ends

;-------------------------------------------------------------------------------------------
;定义栈信息
stack segment
db 800 dup(?)   ;stack 有200个内存单元
stack ends
;-------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------
;定义代码段，主要函数调用，函数的入口
code segment


$left   equ 75
$right  equ 77
$quit   equ 113

assume cs:code,ds:data,ss:stack

main proc far
start:
    mov ax, data
    mov ds, ax
    mov es, ax


    call block_start ;调用第一个函数

exitq:
    call write_score
    call game_over
    mov ax, 4c00h
    int 21h
main endp
;--------------------------------------------------------------------------------------------------------------------------------------
block_start proc near
    mov ax, data
    mov ds, ax
    call clear_vga      ;清屏
    call display_menu   ;显示主菜单
 	call init_game      ;显示光标的位置

    ;call read_score
    ;call write_score

    call chose_mode     ;选择模式

 	call display_figure ;显示游戏开始界面
    call read_score
    call cursor_init    ;设置光标位置
    call start_game     ;开始游戏

    mov ax, 4c00h
    int 21h

block_start endp
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------
;清屏操作
clear_vga proc near
    push ds
    push ax
    push es
    mov ax, 0b800h
    mov es, ax
    mov bx, 0
    mov cx, 2000
blank:
    mov byte ptr es:[bx], ' '
    mov byte ptr es:[bx+1], 07h
    add bx, 2
   loop blank
    pop es
    pop ax
    pop ds

    ret
clear_vga endp
;------------------------------------------------------------------------------------------



;显示主菜单的程序
display_menu proc near
    mov ax, data
    mov ds, ax
    mov cx, 14
    mov ax, 0b81fh
    mov es, ax
    mov bx, offset menu00
row:
    push cx
    mov cx, 60
    mov si, 0h
coll:
    mov al, [bx]
    mov es:[si], al
    cmp al, 2ah ;表示星号
    je color11
    cmp al, '@'
    je color12
    jmp color13
color11:
    mov dl, 00001110b
    jmp _color
color12:
    mov dl, 00001101b
    jmp _color
color13:
    mov dl, 00001010b
    jmp _color
color14:
    mov dl, 04h
_color:
    mov ah, dl
    mov es:[si+1], ah
    inc bx
    add si, 2
    loop coll
    pop cx

    mov ax, es
    ; add ax, 0ah

    add ax, 0ah
    mov es, ax;es+0a :跳下一行
    add bx, 3
    loop row

    ret
display_menu endp

;-----------------------------------------------------------------------------------------------
;处理光标和输入任意键的问题
init_game proc near
    push es
    push ax
    push dx
    push bx
    mov ax, data
    mov ds, ax
    mov ah, 02h
    mov bh, 0
    mov dh, 15
    mov dl, 55
    int 10h

;'mov ah, 0
;    int 16h

    pop bx
    pop dx
    pop ax
    pop es

    ret
init_game endp
;-------------------------------------------------------------------------------------------

;-------------------------------------------------------------------------------------------
;显示游戏开始的界面
display_figure proc near
    push ax
    push cx
    push si
    push es
    push di
    mov ax, data
    mov ds, ax
    call clear_vga
    mov cx, 22
    mov ax, 0b81fh
    mov es, ax
    mov bx, offset figure00
row1:
    push cx
    mov cx, 60
    mov si, 0h
coll1:
    mov al, [bx]
    mov es:[si], al
    cmp al, 2ah ;表示星号
je color21
    cmp al, 40h
je color22
    jmp color23
color21:
    mov dl, 00001110b
    jmp _color1
color22:
    mov dl, 02h
    jmp _color1
color23:
    mov dl, 00001101b
    jmp _color1
color24:
    mov dl, 00001101b
_color1:
    mov ah, dl
    mov es:[si+1], ah
    inc bx
    add si, 2
    loop coll1
    pop cx

    mov ax, es
    add ax, 0ah
    mov es, ax
    add bx, 3
loop row1

    pop di
    pop es
    pop si
    pop cx
    pop ax
    ret
display_figure endp
;-----------------------------------------------------------------------------------------

;-----------------------------------------------------------------------------------------
;画出随机的方块
rand_block proc near

    mov ax, data
    mov ds, ax
    mov now_block[0],1
    mov now_block[1],44  ;初始位置

    xor ax,ax

    mov ax,0b81fh
    add ax,0ah;
    add ax,0ah
    add ax,0ah
    mov es,ax
    mov cx,36
    mov si,4
tops:
    mov bl,es:[si]
    cmp bl,' '
    je outs
    ; call game_over
	jmp exitq
outs:
    add si,2
    loop tops

    xor si,si
    xor ax,ax
    xor bx,bx

get_rand:

    mov al,0
    out 70h,al
    in  al,71h

    mov ah,al
    mov cl,4
    shr ah,cl
    and al,00001111b
    add ah,30h
    add al,30h
    mov bx,0b800h
    mov es,bx

    ;mov byte ptr es:[160*12+40*2],ah
    ;mov byte ptr es:[160*12+40*2+2],al

    and al,00000111b
    mov ah,0
    mov small_block[0],al
	;call remove_small
	;call show_small
    ;xor ax,ax;test line !!!
    ;mov ax,1

    cmp ax,0  ;比较随机值得出我们需要画的方块
    je  get_seq
    cmp ax,1  ;
    je  get_line;
    cmp ax,2  ;
    je  get_L;
    cmp ax,3
    je  get_L;
    cmp ax,4
    je  get_T
    cmp ax,5
    je  get_T
    cmp ax,6
    je  get_line
    cmp ax,7
    je  get_seq


get_seq:

    ;mov ax,0b800h
    ;mov es,ax
    ;mov si,0
    ;mov ah,'#'
    ;mov es:[si],ah

    mov ax,offset square_block ;将方块的图像地址
    mov bx,offset choice_block
    mov cx,data
    mov ds,cx
    mov ds:[bx],ax
    xor ax,ax
    xor bx,bx
    xor cx,cx
    xor dx,dx
    xor si,si
    call draw_block
    jmp over

get_line:

    ;mov ax,0b800h
    ;mov es,ax
    ;mov si,0
    ;mov ah,'@'
    ;mov es:[si],ah

    mov ax,offset line_block ;将方块的图像地址
    mov bx,offset choice_block
    mov cx,data
    mov ds,cx
    mov ds:[bx],ax
    xor ax,ax
    xor bx,bx
    xor cx,cx
    xor dx,dx
    xor si,si
    call draw_block
    jmp over

get_L:

    ;mov ax,0b800h
    ;mov es,ax
    ;mov si,0
    ;mov ah,'!'
    ;mov es:[si],ah
    mov small_block[0],2
    mov ax,offset L_block ;将方块的图像地址
    mov bx,offset choice_block
    mov cx,data
    mov ds,cx
    mov ds:[bx],ax
    xor ax,ax
    xor bx,bx
    xor cx,cx
    xor dx,dx
    xor si,si
    call draw_block
    jmp over

get_T:

    ;mov ax,0b800h
    ;mov es,ax
    ;mov si,0
    ;mov ah,'%'
    ;mov es:[si],ah

    mov ax,offset T_block ;将方块的图像地址
    mov bx,offset choice_block
    mov cx,data
    mov ds,cx
    mov ds:[bx],ax
    xor ax,ax
    xor bx,bx
    xor cx,cx
    xor dx,dx
    xor si,si
    call draw_block
    jmp over

get_Z:

    ;mov ax,0b800h
    ;mov es,ax
    ;mov si,0
    ;mov ah,'%'
    ;mov es:[si],ah

    mov ax,offset Z_block ;将方块的图像地址
    mov bx,offset choice_block
    mov cx,data
    mov ds,cx
    mov ds:[bx],ax
    xor ax,ax
    xor bx,bx
    xor cx,cx
    xor dx,dx
    xor si,si
    call draw_block
    jmp over

over:


    ret

rand_block endp

;----------------------------------------------------------------------------------------

draw_block proc near ;我们将画图函数单独抽象出来


    mov ax,0b81fh  ;设置初始位置，游戏边框顶端

    mov es,ax

    xor ax,ax
    mov ax,data    ;设置数据段
    mov ds,ax


    xor cx,cx
    xor di,di
    ;mov di,offset now_block    ;设置应当开始画的位置，这里只获取行的位置
    mov cl,now_block[0]
    xor bx,bx
    mov bl,now_block[1]
    mov ax,es

add_row:
     add ax,0ah
     loop add_row

     mov es,ax

printf:
     xor cx,cx
     xor ax,ax
     xor di,di
     ;mov di,0
     mov si,offset choice_block
     mov di,ds:[si]
     mov cx,ds:[di]

     ;add di,9
	 ;mov who_block[0],ds:[di]
	 ;mov di,0

     add di,1
     mov si,0


;testu:
    ; mov ah,'@'
     ;mov es:[si],ah
     ;mov al,01h
     ;mov es:[si+1],al
     ;add si,2
     ;loop testu
     ;jmp done

	 in  al,40h              ;开始随机选择方块类型,al=时间随机值
     and al,0111b
 	 mov dl, al
     mov block_colar[0],dl

drawf:
    xor dx,dx
    xor ax,ax

    mov dl,ds:[di] ;row
    mov dh,ds:[di+1];col

    mov ax,160
    mul dl
    mov si,ax

    xor ax,ax
    mov ax,2
    mul dh
	add si,ax
    ;xor dx,dx



    mov ah,'@'
    mov byte ptr es:[bx + si],ah

	xor dx,dx
	mov dl,block_colar[0]
	cmp dx,0
	je  clor1
	cmp dx,1
	je  clor2
	cmp dx,2
	je  clor3
	cmp dx,3
	je  clor4
	cmp dx,4
	je  clor5
	cmp dx,5
	je  clor6
	cmp dx,6
	je  clor7
    cmp dx,7
    je  clor8
clor1:
     mov al,00001001b
	 jmp Zclor
clor2:
     mov al,00001010b
	 jmp Zclor
clor3:
     mov al,00001011b
	 jmp Zclor

clor4:
     mov al,00001100b
     jmp Zclor
clor5:
    mov al,00001101b
    jmp Zclor

clor6:
    mov al,00001110b
    jmp Zclor
clor7:
    mov al,00001111b
    jmp Zclor
clor8:
    mov al,00001101b
    jmp Zclor
Zclor:
	;mov al, 03h
    mov byte ptr es:[bx +  si + 1],al
    add di,2
    loop drawf




done:
    ret



draw_block endp

;-----------------------------------------------------------------------------------------
cursor_init proc near
    push es
    push ax
    push dx
    push bx

    mov ah, 02h
    mov bh, 0
    mov dh, 23
    mov dl, 62
    int 10h

    mov ah, 0
    int 16h

    pop bx
    pop dx
    pop ax
    pop es

    ret

cursor_init endp

;--------------------------------------------------------------------------------------
start_game proc near
    call get_score
    mov ax, data
    mov ds, ax
    mov first_move[0],0
    xor ax,ax
    xor bx,bx
    xor cx,cx
    xor dx,dx

    call show_time

	call remove_small

    call rand_block ;获得一个随机的方块
	call remove_small
	call show_small

    call move_block
 ;   mov cx,17
;l0:
 ;   push cx
 ;   mov cx,0ah
;l1:
  ;  push cx
  ;  mov cx,0ffffh
;l2:
   ; mov ax,0
   ; loop l2
  ;  pop cx
 ;   loop l1
  ;  call move_block
  ;  pop cx
  ;  loop l0



    call get_score
    jmp start_game ;jmp才行，call不行
    ret
start_game endp

;---------------------------------------------------------------------------------------
move_block proc near

    mov cx,20
l1:
    push cx
    ;cmp approve[0],0
    ;jne dback

    call  remove_block;
    add now_block[0],1

    call redraw_block;
    mov ah,1
    int 16h
    mov al,ah
    jz short T1

there2:
    ;sub now_block[0],1

    call remove_block;
    xor ax,ax
    int 16h
    mov bl,al
    mov al,ah
    cmp al,$left
    je get_left
    cmp al,$right
    je get_right
    cmp bl,113
    je exitbq


    cmp al,80
    je  get_done
    call block_change
    ;jmp T1

get_left:
    sub now_block[1],2
    call judge_block
    cmp approve[0],0   ;0 is ok 1 is no
    je  T
    add now_block[1],2
    jmp T
get_right:
    add now_block[1],2
    call judge_block
    cmp approve[0],0   ;0 is ok 1 is no
    je  T
    sub now_block[1],2
    jmp T
get_done:
    add now_block[0],1
    jmp T
;get_up:
    ;call block_change
    ;jmp T1

T:

    call redraw_block;
T1:

    mov cx,0fh ; speed !!!!!!

l:
    push cx
    mov cx,0ffffh
lo:
    mov ax,0
    loop lo

    pop cx
    loop l

    pop cx

    loop l1


dback:
    ret
exitbq:
    jmp exitq
    mov ax, 4c00h
	int 21h

move_block endp


;--------------------------------------------------------------------------------------
redraw_block proc near



    call draw_block;

    ret

redraw_block endp
;-------------------------------------------------------------------------------------

judge_block proc near

    mov ax,0b81fh  ;设置初始位置，游戏边框顶端
    mov es,ax

    xor ax,ax
    mov ax,data    ;设置数据段
    mov ds,ax

    xor cx,cx
    xor di,di
    ;mov di,offset now_block    ;设置应当开始画的位置，这里只获取行的位置
    mov cl,now_block[0]
    xor bx,bx
    mov bl,now_block[1]
    mov ax,es

add_r:
     add ax,0ah
     loop add_r

     mov es,ax


     xor cx,cx
     xor ax,ax
     xor di,di

     mov si,offset choice_block
     mov di,ds:[si]
     mov cx,ds:[di]

     add di,1
     mov si,0


dra:
    xor dx,dx
    xor ax,ax

    mov dl,ds:[di] ;row
    mov dh,ds:[di+1];col

    mov ax,160
    mul dl
    mov si,ax

    xor ax,ax
    mov ax,2
    mul dh

    add si,ax
    xor ax,ax

    mov ax,es:[bx + si]

    cmp al,' '
    jne  set_right

    add di,2
    loop dra
    mov approve[0],0
    jmp gone



set_right:

	mov approve[0],1


gone:

    ret


judge_block endp



;-------------------------------------------------------------------------------------


remove_block proc near

    call show_time

    call remove_block2
    cmp first_move[0],0
    je  there1
    add now_block[0],1
    call judge_block
    cmp approve[0],0
    je there
    sub now_block[0],1
    cmp choice_block[0],1
    jne  yue
    add now_block[0],1
yue:
    call redraw_block
    jmp far ptr start_game
    ;mov approve[0],1
    ;jmp done2

there:
    sub now_block[0],1


there1:
    mov first_move[0],1

    mov ax,0b81fh  ;设置初始位置，游戏边框顶端
    mov es,ax

    xor ax,ax
    mov ax,data    ;设置数据段
    mov ds,ax

    xor cx,cx
    xor di,di
    mov cl,now_block[0]
    xor bx,bx
    mov bl,now_block[1]
    mov ax,es

add_row2:
     add ax,0ah
     loop add_row2

     mov es,ax

printR:
     xor cx,cx
     xor ax,ax
     xor di,di

     mov si,offset choice_block
     mov di,ds:[si]
     mov cx,ds:[di]

     add di,1
     mov si,0


;testu:
     ;mov ah,'@'
     ;mov es:[si],ah
     ;mov al,01h
     ;mov es:[si+1],al
     ;add si,2
     ;loop testu
     ;jmp done

movef:
    xor dx,dx
    xor ax,ax

    mov dl,ds:[di] ;row
    mov dh,ds:[di+1];col

    mov ax,160
    mul dl
    mov si,ax

    xor ax,ax
    mov ax,2
    mul dh

    add si,ax
    mov ah,' '
    mov byte ptr es:[bx + si],ah
    mov al,0h
    mov byte ptr es:[bx +  si + 1],al
    add di,2
    loop movef


done2:

    ret


remove_block endp

;---------------------------------------------------------------------------------------
remove_block2 proc near


    mov ax,0b81fh  ;设置初始位置，游戏边框顶端
    mov es,ax

    xor ax,ax
    mov ax,data    ;设置数据段
    mov ds,ax

    xor cx,cx
    xor di,di
    mov cl,now_block[0]
    xor bx,bx
    mov bl,now_block[1]
    mov ax,es

add_row3:
     add ax,0ah
     loop add_row3

     mov es,ax


     xor cx,cx
     xor ax,ax
     xor di,di

     mov si,offset choice_block
     mov di,ds:[si]
     mov cx,ds:[di]

     add di,1
     mov si,0


movefv:
    xor dx,dx
    xor ax,ax

    mov dl,ds:[di] ;row
    mov dh,ds:[di+1];col

    mov ax,160
    mul dl
    mov si,ax

    xor ax,ax
    mov ax,2
    mul dh

    add si,ax
    mov ah,' '
    mov byte ptr es:[bx + si],ah
    mov al,0h
    mov byte ptr es:[bx +  si + 1],al
    add di,2
    loop movefv


done3:

    ret


remove_block2 endp
;----------------------------------------------------------------------------------------

get_score proc near

    mov ax,0b800h
    mov es,ax
    mov ah,'@'
    mov es:[0],ah
    xor ax,ax
    mov ax,0b81fh


    mov es,ax

    mov cx,20
    mov di,0
frow:
    xor ax,ax

    push cx
    mov  bx,offset now_line
    mov  ax,data
    mov  ds,ax
    mov  ds:[bx],cx
    mov ax,0b81fh

colute:
     add ax,0ah
     loop colute
     mov es,ax
     ;mov ah,'!'   ;test
     ;mov si,0
     ;mov es:[si],ah
     xor cx,cx
     mov cx,32  ;36
     mov si,14  ;4
fcow:
     mov al,' '
     cmp es:[si],al  ;test every line is ok
     je breakk
     add si,2
     loop fcow
     call clear_a
     ;call add_score


breakk:

     pop cx
     sub cx,1
     cmp cx,0
     jne frow
     ;loop frow

done5:
     ret


get_score endp


;-----------------------------------------------------------------------------------------
clear_a proc near

      xor ax,ax
      xor cx,cx
      mov bx,offset now_line
      mov ax,data
      mov ds,ax
      mov cx,ds:[bx]
      mov dx,cx
      mov ax,0b81fh
      mov es,ax
      mov si,0
tes:
      add ax,0ah
      loop tes

      mov es,ax

      mov ax,es
      mov ds,ax
      sub ax,0ah
      mov ds,ax

      mov si,14;4
      mov di,14;4
      mov cx,dx
      sub cx,5
clx:
      push cx
      mov cx,32; 36
cll:
      mov al,'*'
      cmp byte ptr es:[si],al
      je lol
      mov bx,ds:[si]
      mov es:[di],bx
      add di,2
      add si,2
      loop cll

      mov ax,es
      sub ax,0ah
      mov es,ax

      mov ax,ds
      sub ax,0ah
      mov ds,ax

      mov di,14
      mov si,14


      pop cx

      loop clx


lol:

      mov ax,data
      mov ds,ax
      xor ax,ax

      call add_score
      ret

clear_a endp

;-----------------------------------------------------------------------------------------

game_over proc near
	mov ax, data
	mov ds, ax
	call clear_vga
	mov cx, 21
	mov ax, 0b81fh
	mov es, ax
	mov bx, offset g_over01
	row11:
	push cx
	mov cx, 60
	mov si, 0h
	coll11:
	mov al, [bx]
	mov es:[si], al
	cmp al, 2ah ;表示星号
	je color211
	cmp al, 40h
	je color221
	jmp color231
	color211:
	mov dl, 00001110b
	jmp _color11
	color221:
	mov dl, 00001110b
	jmp _color11
	color231:
	mov dl, 00001010b
	jmp _color11
	color241:
	mov dl, 00001010b
	_color11:
	mov ah, dl
	mov es:[si+1], ah
	inc bx
	add si, 2
	loop coll11
	pop cx

	mov ax, es
	add ax, 0ah
	mov es, ax
	add bx, 3
	loop row11


ret

game_over endp


;----------------------------------------------------------------------------------------
block_change proc near




	mov ax,data
	mov ds,ax
	mov si,offset choice_block
    mov di,ds:[si]
    xor ax,ax
    mov ax,di
    mov last_choice[0],al
    mov cx,ds:[di]
    mov si,0
    mov ax,0b800h
    mov es,ax
    add di,9
    xor cx,cx
    mov cl,ds:[di]

	cmp cl,1
	je  change_1
	cmp cl,2
	je  change_2
    cmp cl,3
    je  change_3
    cmp cl,4
    je  change_4
    cmp cl,5
    je  change_5
    cmp cl,6
    je  change_6
    cmp cl,7
    je change_7
    cmp cl,8
    je change_8

    cmp cl, 0
    je change_0

change_0:

    call redraw_block
    jmp donex

change_1:



    mov ax,offset line_block2 ;将方块的图像地址
    mov bx,offset choice_block
    ; mov cx,data
    ; mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex



change_2:
    ;
    ; xor ax,ax
    ; xor bx,bx
    ; xor cx,cx

    mov ax,offset line_block ;将方块的图像地址
    mov bx,offset choice_block
    ; mov cx,data
    ; mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex

change_3:

    ; xor ax,ax
    ; xor bx,bx
    ; xor cx,cx

    mov ax,offset L_block2 ;将方块的图像地址
    mov bx,offset choice_block
    ; mov cx,data
    ; mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex

change_4:

    ; xor ax,ax
    ; xor bx,bx
    ; xor cx,cx

    mov ax,offset L_block ;将方块的图像地址
    mov bx,offset choice_block
    ; mov cx,data
    ; mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex

change_5:

    ; xor ax,ax
    ; xor bx,bx
    ; xor cx,cx

    mov ax,offset T_block2 ;将方块的图像地址
    mov bx,offset choice_block
    mov cx,data
    mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex
change_6:

    ; xor ax,ax
    ; xor bx,bx
    ; xor cx,cx

    mov ax,offset T_block ;将方块的图像地址
    mov bx,offset choice_block
    ; mov cx,data
    ; mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex

change_7:

    ; xor ax,ax
    ; xor bx,bx
    ; xor cx,cx

    mov ax,offset L_block2 ;将方块的图像地址
    mov bx,offset choice_block
    ; mov cx,data
    ; mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex


change_8:

    ; xor ax,ax
    ; xor bx,bx
    ; xor cx,cx

    mov ax,offset L_block ;将方块的图像地址
    mov bx,offset choice_block
    ; mov cx,data
    ; mov ds,cx
    mov ds:[bx],ax
    ;call redraw_block;
    jmp donex

donex:
    xor ax,ax
    call judge_block
    cmp approve[0],0
    jne  okk

    call redraw_block
    jmp T1
okk:
    xor si,si
    xor di,di
    xor ax,ax
    mov si,offset last_choice
    mov di,offset choice_block
    mov ax,ds:[si]
    mov ds:[di],ax
	jmp T1


block_change endp

;-------------------------------------------------------------------------------------------

chose_mode proc near   ;显示主菜单

    mov ah, 0

    int 16h
    mov bl,al
    mov al,ah
    cmp bl,50
    je over_end



over_start:
    ret
over_end:
    jmp exitq


chose_mode endp
;-------------------------------------------------------------------------------------------
show_time  proc near

    mov al,4
    out 70h,al
    in al,71h
    mov ah,al
    mov cl,4
    shr ah,cl
    and al,00001111b
    add ah,30h
    add al,30h

    mov bx,0b800h
    mov es,bx

    mov byte ptr es:[160*12 + 55*2  ],ah
    mov byte ptr es:[160*12 + 55*2 + 2],al
;--------
    xor ax,ax
    mov ah,':'
    mov al,02h
    mov byte ptr es:[160*12 + 57*2],ah
    mov byte ptr es:[160*12 + 57*2 + 1],al
    xor ax,ax

    mov al,2
    out 70h,al
    in al,71h
    mov ah,al
    mov cl,4
    shr ah,cl
    and al,00001111b
    add ah,30h
    add al,30h

    mov bx,0b800h
    mov es,bx

    mov byte ptr es:[160*12 + 59*2 ],ah
    mov byte ptr es:[160*12 + 59*2 + 2],al
;--------
    xor ax,ax
    mov ah,':'
    mov al,02h
    mov byte ptr es:[160*12 + 61*2],ah
    mov byte ptr es:[160*12 + 61*2 + 1],al
    xor ax,ax

    mov al,0
    out 70h,al
    in al,71h
    mov ah,al
    mov cl,4
    shr ah,cl
    and al,00001111b
    add ah,30h
    add al,30h

    mov bx,0b800h
    mov es,bx

    mov byte ptr es:[160*12 + 63*2 ],ah
    mov byte ptr es:[160*12 + 63*2 + 2],al


    ret

show_time endp

;------------------------------------------------------------------------------------------------------

add_score proc near

	xor ax,ax
	mov al,score[0]
	add al,1

	mov cx,0b800h
	mov es,cx
    add al,48
	mov ah,02h
	mov byte ptr es:[160*14 + 57*2],al
	mov byte ptr es:[160*14 + 57*2 + 1],ah
    mov score[0],al
	mov al,'0'
	mov ah,02h
	mov byte ptr es:[160*14 + 59*2],al
	mov byte ptr es:[160*14 + 59*2 + 1],ah

	mov byte ptr es:[160*14 + 61*2],al
	mov byte ptr es:[160*14 + 61*2 + 1],ah

	ret


add_score endp

;------------------------------------------------------------------------------------------------------

show_small	proc near
    mov ax,0b800h
    mov es,ax
    xor ax,ax
    mov al,'@'
    mov ah,02h
    mov bl,small_block[0]
    cmp bl,0
    jne  show0
    jmp show_0
show0:
   cmp bl,1
   jne  show1
   jmp show_1
show1:
   cmp bl,2
   jne  show2
   jmp show_2
show2:
   cmp bl,3
   jne  show3
   jmp show_3
show3:
   cmp bl,4
   jne  show4
   jmp show_4
show4:
   jmp show_1


   ;cmp ax,0  ;比较随机值得出我们需要画的方块
   ;je  get_seq
   ;cmp ax,1  ;
   ;je  get_line;
   ;cmp ax,2  ;
   ;je  get_L;
   ;cmp ax,3
   ;je  get_Z;
   ;cmp ax,4
   ;je  get_T
   ;jmp get_line;




show_1:
	mov es:[160*5 + 57*2],al
	mov es:[160*5 + 57*2 +1],ah
	mov es:[160*6 + 57*2],al
	mov es:[160*6 + 57*2 +1],ah
	mov es:[160*7 + 57*2],al
	mov es:[160*7 + 57*2 +1],ah
	mov es:[160*8 + 57*2],al
	mov es:[160*8 + 57*2 +1],ah
    jmp ok_s

show_2:
	mov es:[160*5 + 57*2],al
	mov es:[160*5 + 57*2 +1],ah
	mov es:[160*6 + 57*2],al
	mov es:[160*6 + 57*2 +1],ah
	mov es:[160*7 + 57*2],al
	mov es:[160*7 + 57*2 +1],ah
	mov es:[160*7 + 59*2],al
	mov es:[160*7 + 59*2 +1],ah
	jmp ok_s

show_3:
	mov es:[160*5 + 57*2],al
	mov es:[160*5 + 57*2 +1],ah
	mov es:[160*6 + 57*2],al
	mov es:[160*6 + 57*2 +1],ah
	mov es:[160*6 + 59*2],al
	mov es:[160*6 + 59*2 +1],ah
	mov es:[160*7 + 59*2],al
	mov es:[160*7 + 59*2 +1],ah
    jmp ok_s

show_4:
	mov es:[160*5 + 57*2],al
	mov es:[160*5 + 57*2 +1],ah
	mov es:[160*6 + 57*2],al
	mov es:[160*6 + 57*2 +1],ah
	mov es:[160*7 + 57*2],al
	mov es:[160*7 + 57*2 +1],ah
	mov es:[160*6 + 59*2],al
	mov es:[160*6 + 59*2 +1],ah
    jmp ok_s

show_0:
	mov es:[160*5 + 57*2],al
	mov es:[160*5 + 57*2 +1],ah
	mov es:[160*5 + 59*2],al
	mov es:[160*5 + 59*2 +1],ah
	mov es:[160*6 + 57*2],al
	mov es:[160*6 + 57*2 +1],ah
	mov es:[160*6 + 59*2],al
	mov es:[160*6 + 59*2 +1],ah
    jmp ok_s

ok_s:
    ret


show_small endp

;-----------------------------------------------------------------------------------------

remove_small proc near
	mov ax,0b800h
	mov es,ax
	xor ax,ax
	mov al,' '
	mov ah,0h

	xor bx,bx
	mov bl,small_block[0]
	; cmp bl,0
	; jne  show00
	; jmp show_00
	; show00:
	; cmp bl,1
	; jne  show11
	; jmp show_11
	; show11:
	; cmp bl,2
	; jne  show22
	; jmp show_22
	; show22:
	; cmp bl,3
	; jne  show33
	; jmp show_33
	; show33:
	; cmp bl,4
	; jne  show44
	; jmp show_44
	; show44:
	; jmp show_11

show_11:
     mov es:[160*5 + 57*2],al
     mov es:[160*5 + 57*2 +1],ah
     mov es:[160*6 + 57*2],al
     mov es:[160*6 + 57*2 +1],ah
     mov es:[160*7 + 57*2],al
     mov es:[160*7 + 57*2 +1],ah
     mov es:[160*8 + 57*2],al
     mov es:[160*8 + 57*2 +1],ah
 ;jmp ok_s2

show_22:
     mov es:[160*5 + 57*2],al
     mov es:[160*5 + 57*2 +1],ah
     mov es:[160*6 + 57*2],al
     mov es:[160*6 + 57*2 +1],ah
     mov es:[160*7 + 57*2],al
     mov es:[160*7 + 57*2 +1],ah
     mov es:[160*7 + 59*2],al
     mov es:[160*7 + 59*2 +1],ah
 ;jmp ok_s2

show_33:
     mov es:[160*5 + 57*2],al
     mov es:[160*5 + 57*2 +1],ah
     mov es:[160*6 + 57*2],al
     mov es:[160*6 + 57*2 +1],ah
     mov es:[160*6 + 59*2],al
     mov es:[160*6 + 59*2 +1],ah
     mov es:[160*7 + 59*2],al
     mov es:[160*7 + 59*2 +1],ah
     ; jmp ok_s2

show_44:
     mov es:[160*5 + 57*2],al
     mov es:[160*5 + 57*2 +1],ah
     mov es:[160*6 + 57*2],al
     mov es:[160*6 + 57*2 +1],ah
     mov es:[160*7 + 57*2],al
     mov es:[160*7 + 57*2 +1],ah
     mov es:[160*6 + 59*2],al
     mov es:[160*6 + 59*2 +1],ah
 ; jmp ok_s2

show_00:
    mov es:[160*5 + 57*2],al
    mov es:[160*5 + 57*2 +1],ah
    mov es:[160*5 + 59*2],al
    mov es:[160*5 + 59*2 +1],ah
    mov es:[160*6 + 57*2],al
    mov es:[160*6 + 57*2 +1],ah
    mov es:[160*6 + 59*2],al
    mov es:[160*6 + 59*2 +1],ah
 ; jmp ok_s2

ok_s2:
    ret

remove_small endp

;-----------------------------------------------------------------------------------------

read_score proc near

    mov ax, data
    mov ds, ax

    ;打开文件TEST.TXT
    mov ah,3DH
    lea dx,path1
    mov al,2
    int 21H
    jc gones


    ;读取文件内容，存入buffer1
    lea dx,buffer1
    mov bx,ax
    mov cx,1
    mov ah,3FH
    int 21H

    ; ;显示buffer1中的内容
    ; mov cx,ax
    ; lea si,buffer1
    ; NEXT:
    ; mov dl,[si]
    ; mov ah,2
    ; int 21H
    ; INC si
    ; LOOP NEXT
    xor ax,ax
	mov al,buffer1[0]

	mov cx,0b800h
	mov es,cx
	mov ah,02h
	mov byte ptr es:[160*10 + 61*2],al
	mov byte ptr es:[160*10 + 61*2 + 1],ah
	mov al,'0'
	mov ah,02h
	mov byte ptr es:[160*10 + 63*2],al
	mov byte ptr es:[160*10 + 63*2 + 1],ah

	mov byte ptr es:[160*10 + 65*2],al
	mov byte ptr es:[160*10 + 65*2 + 1],ah
gones:
	ret

    ;关闭文件
    mov ah,3EH
    int 21H

    ret


read_score endp
;-----------------------------------------------------------------------------------------
write_score proc near
    mov ax, data
    mov ds, ax

    xor ax, ax
    xor bx,bx
    mov al,buffer1[0]
    mov score[0],0

    cmp al,score[0]
    jbe oover
    xor ax, ax

    ;打开文件TEST.TXT
    mov ah,3DH
    lea dx,path1
    mov al,2
    int 21H
    jc oover
    ; mov dx,200H
    ; mov bx,0
    ; mov ds,bx
    mov dx,offset score

    mov bx,ax
    mov cx,1
    mov ah,40H
    int 21H

    mov ax, data
    mov ds, ax
    mov ah,3EH
    int 21H
oover:
    ret

write_score endp
;-----------------------------------------------------------------------------------------


code ends
end start
