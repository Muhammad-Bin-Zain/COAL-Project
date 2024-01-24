[org 0x0100]

jmp start

line1: db "Project ",0
 line2: db  "Janaza",0
line3: db "Developed by Gora Sahb & Benzeze",0
line4: db "  Press Any key  ",0
page21: db " Instructions ",0
page22: db "=> Use A , S , D keys to control the Shapes ",0
page23: db "=> 10 Score will awarded if you have completed the row ",0
gameover: dw 0
; sec: dw 3200
sec1: dw 600
tickcount: dw 0
oldisr: dd 0
randnum: dw 0
rand :dw 0
score: dw 0
time : dw 0
timeStr: db " Time :",0
scoreStr: db "Score :",0
UpcomingShapeStr: db "Upcoming Shape:",0
EndStr: db"Game End:",0
YourScoreStr: db"Your Score:",0
randNum: dw 0
currentShape: dw 0
tempds: dw 0



randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next

  MOV     AH, 00h   ; interrupt to get system timer in CX:DX 
  INT     1AH
  inc word [rand]
  mov     [randnum], dx
  jmp next1

  next:
  mov     ax, 25173          ; LCG Multiplier
  mul     word  [randnum]     ; DX:AX = LCG multiplier * seed
  add     ax, 13849          ; Add LCG increment value
  ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
  mov     [randnum], ax          ; Update seed = return value

 next1:xor dx, dx
 mov ax, [randnum]
 mov cx, [bp+4]
 inc cx
 div cx
 
 mov [bp+6], dx
 popa
 pop bp
 ret 2


clrscr: 
	push es
	push ax
	push di
	mov ax, 0xb800
 	mov es, ax ; point es to video base
 	mov di, 0 ; point di to top left column
nextloc:
	mov word [es:di], 0x0720 ; clear next char on screen
	add di, 2 ; move to next screen location
	cmp di, 4000 ; has the whole screen cleared
	jne nextloc ; if no clear next position
 	pop di
	pop ax
 	pop es
 	ret 

colorscr: 
	push bp
	mov bp,sp
	push es
	push ax
	push di
	mov ax, 0xb800
 	mov es, ax	; point es to video base
 	mov di, 0	; point di to top left column
	mov ah, [bp+4]
	mov al,0x2A
nextloc1:
	; mov word [es:di], 0x6720	;clear next char on screen
	mov word [es:di], ax	;clear next char on screen
	add di, 2	;move to next screen location
	cmp di, 4000	;has the whole screen cleared
	jne nextloc1	;if no clear next position
 	
	pop di
	pop ax
 	pop es
	mov sp,bp
	pop bp
 	ret 2

;PRINTING OF GREY BACKGROUND STARTS
print:
	push bp
	mov bp,sp

	mov ax, 0xb800
	mov es,ax
	mov al,80
	mul byte[bp+6]
	add ax,[bp+4]
	shl ax,1
	mov di,ax
	mov bx,0	
	mov cx,20	;length of the game screen 

subp1:
	mov word[es:di], 0x7020
	add di,2
	add bx,1
	cmp bx,40	;width of the game screen
	jne subp1

	mov di,ax
	add di,160
	mov ax,di
	mov bx,0
	sub cx,1
	cmp cx,0
	jz end1
	jnz subp1

end1:

	pop bp
	ret 4
;PRINTING OF GREY BACKGROUND ENDS


;PRINTING OF SHAPES START
shape1:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	;push di
	;push es
	mov ax,0xb800
	mov es,ax
	mov al,80
	mul byte[bp+8]
	;mul byte[bp+6]
	add ax,[bp+6]
	;add ax,[bp+4]
	
	shl ax,1
	mov di,ax
	;si value below 2 lines
	;mov si,di
	;sub si,2
	mov ax,[bp+4]
	mov cx,3
	push cx
	
	
vertical:
	;add di,160
	mov word[es:di],ax
	add di,160
	dec cx
	cmp cx,0
	jne vertical
	pop cx
	add cx,1
	sub di,160
	mov si,di
	sub si,2
horizontal:
	mov word[es:di],ax
	add di,2
	dec cx
	cmp cx,0
	jne horizontal
	sub di,2
	
	;pop es
	;pop di
	pop dx
	pop cx
	pop bx
	pop ax
	mov sp,bp
	pop bp
	ret 6
	


shape2:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	;push di
	;push es
	
	mov ax,0xb800
	mov es,ax
	mov al,80
	mul byte[bp+8]
	;mul byte[bp+6]
	add ax,[bp+6]
	;add ax,[bp+4]
	shl ax,1
	mov di,ax
	; mov si,di
	; sub si,2
	;mov ah,[bp+4] 
	; mov ah,0x33
	; ;mov al,0x20
	; mov al,0x32
	mov ax,[bp+4]
	mov cx,3

vertical2:
	mov word[es:di],ax
	add di,160
	dec cx
	cmp cx,0
	jne vertical2
	sub di,160
	mov si,di
	sub si,2
	;pop es
	;pop di
	pop dx
	pop cx
	pop bx
	pop ax
	mov sp,bp
	pop bp
	ret 6
	
	
shape3:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	;push di
	
	mov ax,0xb800
	mov es,ax
	mov al,80
	mul byte[bp+8]
	;mul byte[bp+6]
	add ax,[bp+6]
	;add ax,[bp+4]
	shl ax,1
	mov di,ax
	sub di,2
	push di
	; ;mov ah,[bp+4]
	; mov ah,0x44
	; ;mov al,0x20  ;now ax has the attribute and the letter to be printed(i.e space)
	; mov al,0x33		;now ax has the attribute and the letter to be printed(i.e space)
	mov ax,[bp+4]
	mov cx,4
	push cx
	
horizontal1:
	add di,2
	mov word[es:di],ax
	dec cx
	cmp cx,0
	jne horizontal1

	pop cx
	pop di
	add di,160
	mov si,di
	add di,2
horizontal1.1:
	mov word[es:di],ax
	add di,2
	dec cx
	cmp cx,0
	jne horizontal1.1
	sub di,2
	;pop di
	pop dx
	pop cx
	pop bx
	pop ax
	mov sp,bp
	pop bp
	ret 6


shape4:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	;push di
	
	mov ax,0xb800
	mov es,ax
	mov al,80
	mul byte[bp+8]
	;mul byte[bp+6]
	add ax,[bp+6]
	;add ax,[bp+4]
	shl ax,1
	mov di,ax
	push di
	; mov ah,[bp+4]   ;attribute
	;mov ah,0x55   ;attribute
	;mov al,0x20     ;space character
	;mov al,0x34
	mov ax,[bp+4]
	mov cx,4
	push cx
	;add di,2
	add di,4
first:	
	mov word[es:di],ax
	add di,2
	dec cx
	cmp cx,0
	jne first
	
	
	pop cx
	pop di
	add di,160
	mov si,di
	sub si,2

second:
	mov word[es:di],ax
	add di,2
	dec cx
	cmp cx,0
	jne second
	add di,2
	
	;pop di
	pop dx
	pop cx
	pop bx
	pop ax
	mov sp,bp
	pop bp
	ret 6

UpcomingShape:
	
	push bp
	mov bp,sp
	push ax
	push es
	push cx
	
	mov ax,0xb800
	mov es,ax
	mov ax, 0xE72A
	mov di,910
	push di
	mov cx,15
	
lus:
	mov word[es:di],ax
	add di,2
	loop lus
	
	pop di
	add di,160
	push di
	mov cx,15
lus1:
	mov word[es:di],ax
	add di,2
	loop lus1

	pop di
	add di,160
	push di
	mov cx,15
lus2:
	mov word[es:di],ax
	add di,2
	loop lus2

	pop di
	add di,160
	push di
	mov cx,15
lus3:
	mov word[es:di],ax
	add di,2
	loop lus3

	pop di
	add di,160
	push di
	mov cx,15
lus4:
	mov word[es:di],ax
	add di,2
	loop lus4	
	
	cmp word[randNum],1
	jz s1
	cmp word[randNum],2
	jz s2
	cmp word[randNum],3
	jz s3
	cmp word[randNum],4
	jz s4
	
	
s1: 
	mov ax,6	;rows coordinate for upcoming 
	push ax
	mov ax,60
	push ax
	mov ax,0x2231  ;attribute
	push ax
	call shape1
	jmp USend
	;call delay
s2: 
	mov ax,6	;rows coordinate for upcoming 
	push ax
	mov ax,60
	push ax
	mov ax,0x3331  ;attribute
	push ax
	call shape2
	jmp USend
	;call delay
s3: 
	mov ax,6	;rows coordinate for upcoming 
	push ax
	mov ax,60
	push ax
	mov ax,0x4431  ;attribute
	push ax
	call shape3
	jmp USend
	;call delay
s4: 
	mov ax,6	;rows coordinate for upcoming 
	push ax
	mov ax,60
	push ax
	mov ax,0x5531  ;attribute
	push ax
	call shape4
	
USend:
	pop cx 
	pop es
	pop ax
	mov sp,bp
	pop bp
	ret
	
;PRINTING OF SHAPES END


;start of bin zain code

printstr: 
	push bp 
	mov bp, sp 
	push es 
	push ax 
	push cx 
	push si 
	push di 
	push ds 
	
	pop es ; load ds in es 
	
	mov di, [bp+4] ; point di to string 
	mov cx, 0xffff ; load maximum number in cx 
	xor al, al ; load a zero in al 
	repne scasb ; find zero in the string 
	mov ax, 0xffff ; load maximum number in ax 
	sub ax, cx ; find change in cx 
	dec ax ; exclude null from length 
	jz exit ; no printing if string is empty
	mov cx, ax ; load string length in cx 
	mov ax, 0xb800 
	mov es, ax ; point es to video base 
	mov al, 80 ; load al with columns per row 
	mul byte [bp+8] ; multiply with y position 
	add ax, [bp+10] ; add x position 
	shl ax, 1 ; turn into byte offset 
	mov di,ax ; point di to required location 
	mov si, [bp+4] ; point si to string 
	mov ah, [bp+6] ; load attribute in ah 
	cld ; auto increment mode 
	nextchar: lodsb ; load next char in al 
	stosw ; print char/attribute pair 
	loop nextchar ; repeat for the whole string 
exit:
	pop di 
	pop si 
	pop cx 
	pop ax 
	pop es 
	pop bp 
	ret 8


DispayActualTime:

	ret



PrintTime:
	push ax
	mov ax ,56 ;[bp+10] x position
	push ax
	mov ax, 15  ;[bp+8] y position
	push ax
	mov ax,0x71	; attribute here [bp+6]
	push ax
	mov ax,timeStr	;[bp+4]
	push ax
	
	call printstr ; printing string of the time

	pop ax
	ret

PrintScore:
	push ax
	mov ax ,56 ;[bp+10] x position
	push ax
	mov ax, 17  ;[bp+8] y position
	push ax
	mov ax,0x71	; attribute here [bp+6]
	push ax
	mov ax,scoreStr	;[bp+4]
	push ax
	
	call printstr ; printing string of the time
	
	

	pop ax
	ret

PrintUpcomingShape:
	push ax
	mov ax ,56 ;[bp+10] x position
	push ax
	mov ax, 4  ;[bp+8] y position
	push ax
	mov ax,0x71	; attribute here [bp+6]
	push ax
	mov ax,UpcomingShapeStr	;[bp+4]
	push ax
	
	call printstr ; printing string of the time
	
	

	pop ax
	ret


;end of bin zain code

PrintAllShapes:
	;CALLING SHAPE 1
	mov ax,19	;rows for shape
	push ax
	mov ax,5	;cols for shape
	push ax
	mov ax,0x2131	;attribute
	push ax
	call shape1

	;CALLING SHAPE 2
	mov ax,19	;rows for shape
	push ax	
	mov ax, 11	;cols for shape
	push ax
	mov ax,0x3131	;attribute
	push ax
	call shape2

	;CALLING SHAPE 3
	mov ax,20	;rows for shape
	push ax	
	mov ax, 15	;cols for shape
	push ax
	mov ax,0x4131  ;attribute
	push ax
	call shape3
	
	mov ax,12	;rows for shape
	push ax	
	mov ax, 18	;cols for shape
	push ax
	mov ax,0x4131  ;attribute
	push ax
	call shape3
	

	;CALLING SHAPE 4
	
	mov ax,20	;rows for shape
	push ax	
	mov ax, 20	;cols for shape
	push ax
	;mov ax,0x50  ;attribute
	;push ax
	mov ax, 0x5131	;attribute + character
	push ax
	call shape4
	

	ret
	
	
delay:

	push ax
	push cx
	mov ax,2500
	mov cx,2500

outloop:
	mov cx,2500
innerloop:
	sub cx,2
	jnz innerloop
	sub ax,2
	jnz outloop

	pop cx
	pop ax
	ret
	

PrintEnd:
	push ax

	mov ax ,35 ;[bp+10] x position
	push ax
	mov ax, 10  ;[bp+8] y position
	push ax
	mov ax,0xD2	; attribute here [bp+6]
	push ax
	; mov ax,EndStr	;[bp+4]
	mov ax,EndStr
	push ax
	call printstr ; printing string of the time

	pop ax
	ret

PrintYourScore:
	push ax
	mov ax ,30 ;[bp+10] x position
	push ax
	mov ax, 12  ;[bp+8] y position
	push ax
	mov ax,0x71	; attribute here [bp+6]
	push ax
	mov ax,YourScoreStr	;[bp+4]
	push ax
	call printstr ; printing string of the time

	pop ax
	ret


shape4Down:
	push bp
	mov bp,sp
	;number check for shape4(Z)
	push ax
	mov al,byte[es:di]
	cmp al,0x31
	pop ax
	jz sh4d
	
	push ax
	sub di,2
	mov al,byte[es:di]
	add di,2
	cmp al,0x31
	pop ax
	jz sh4d
	
	push ax
	sub di,4
	add di,160
	mov al,byte[es:di]
	sub di,160
	add di,4
	cmp al,0x31
	pop ax
	jz sh4d
	
	push ax
	sub di,6
	add di,160
	mov al,byte[es:di]
	sub di,160
	add di,6
	cmp al,0x31
	pop ax
	jz sh4d
	
	push ax
	sub di,8
	add di,160
	mov al,byte[es:di]
	sub di,160
	add di,8
	cmp al,0x31
	pop ax
	jz sh4d
	
	push ax
	sub di,10
	add di,160
	mov al,byte[es:di]
	sub di,160
	add di,10
	cmp al,0x31
	pop ax
	jz sh4d
	;number checks for shape4(Z) end
	jnz sh4dexit
sh4d:
	mov word[bp+4],0
sh4dexit:
	mov sp,bp
	pop bp
	ret
shape4Left:
	push bp
	mov bp,sp
	;shape4(Z) number Check
	push ax
	mov al,byte[ds:si]
	cmp al,0x31
	pop ax
	jz sh4l
	
	push ax
	sub si,160
	add si,4
	mov al,byte[ds:si]
	sub si,4
	add si,160
	cmp al,0x31
	pop ax
	jz sh4l
	jnz sh4lexit
	
sh4l:
	mov word[bp+4],0
sh4lexit:
	mov sp,bp
	pop bp
	ret
	
	
shape4Right:
	push bp
	mov bp,sp
	;shape(Z) number checks
	push ax
	sub di,2
	mov al,byte[es:di]
	add di,2
	cmp al,0x31
	pop ax
	jz sh4r
	
	push ax
	sub di,160
	add di,2
	mov al,byte[es:di]
	sub di,2
	add di,160
	cmp al,0x31
	pop ax
	jz sh4r
	jnz sh4rexit
	
sh4r:
	mov word[bp+4],0
sh4rexit:
	mov sp,bp
	pop bp
	ret

shape3Down:
	push bp
	mov bp,sp
	push ax
	add di,160
	mov al,byte[es:di]
	sub di,160
	cmp al,0x31
	pop ax
	jz sh3d
	
	push ax
	add di,158
	mov al,byte[es:di]
	sub di,158
	cmp al,0x31
	pop ax
	jz sh3d
	
	push ax
	add di,156
	mov al,byte[es:di]
	sub di,156
	cmp al,0x31
	pop ax
	jz sh3d
	
	push ax
	add di,154
	mov al,byte[es:di]
	sub di,154
	cmp al,0x31
	pop ax
	jz sh3d
	jnz sh3dexit
sh3d:
	mov word[bp+4],0
sh3dexit:
	mov sp,bp
	pop bp
	ret
	
	
shape3Left:
	push bp
	mov bp,sp
	;shape3(square) number checks
	push ax
	mov al,byte[ds:si]
	cmp al,0x31
	pop ax
	jz sh3l
	
	push ax
	sub si,160
	mov al,byte[ds:si]
	add si,160
	cmp al,0x31
	pop ax
	jz sh3l
	;shape3 check End
	jnz sh3lexit
sh3l:
	mov word[bp+4],0
sh3lexit:
	mov sp,bp
	pop bp
	ret
	
shape3Right:
	push bp
	mov bp,sp
	;shape3(right) Check
	push ax
	add di,2
	mov al,byte[es:di]
	sub di,2
	cmp al,0x31
	pop ax
	jz sh3r
	
	push ax
	add di,2
	sub di,160
	mov al,byte[es:di]
	add di,158
	cmp al,0x31
	pop ax
	jz sh3r
	jnz sh3rexit
sh3r:
	mov word[bp+4],0
sh3rexit:
	mov sp,bp
	pop bp
	ret

shape2Down:
	;number check for shapee2(I)
	push bp
	mov bp,sp
	push ax
	add di,160
	mov al,byte[es:di]
	sub di,160
	cmp al,0x31
	pop ax
	jz sh2d
	jnz sh2dexit
sh2d:
	mov word[bp+4],0
sh2dexit:
	mov sp,bp
	pop bp
	ret
	
shape2Left:
	push bp
	mov bp,sp
	push ax
	mov al,byte[ds:si]
	cmp al,0x31
	pop ax
	jz sh2l

	push ax
	sub si,160
	mov al,byte[ds:si]
	add si,160
	cmp al,0x31
	pop ax
	jz sh2l
	
	push ax
	sub si,320
	mov al,byte[ds:si]
	add si,320
	cmp al,0x31
	pop ax
	jz sh2l
	jnz sh2lexit
sh2l:
	mov word[bp+4],0
sh2lexit:
	mov sp,bp
	pop bp
	ret	

	
shape2Right:
	push bp
	mov bp,sp
	;shape2(I) number check
	push ax
	add di,2
	mov al,byte[es:di]
	sub di,2
	cmp al,0x31
	pop ax
	jz sh2r
	
	push ax
	add di,2
	sub di,160
	mov al,byte[es:di]
	add di,158
	cmp al,0x31
	pop ax
	jz sh2r
	
	push ax
	add di,2
	sub di,320
	mov al,byte[es:di]
	add di,318
	cmp al,0x31
	pop ax
	jz sh2r
	jnz sh2rexit
sh2r:
	mov word[bp+4],0
sh2rexit:
	mov sp,bp
	pop bp
	ret	

	
shape1Down:
	push bp
	mov bp,sp
	;number checks for shape1(L) and shape3(square) start
	push ax
	add di,160
	mov al,byte[es:di]
	sub di,160
	cmp al,0x31
	pop ax
	jz changeval0
	
	push ax
	add di,158
	mov al,byte[es:di]
	sub di,158
	cmp al,0x31
	pop ax
	jz changeval0
	
	push ax
	add di,156
	mov al,byte[es:di]
	sub di,156
	cmp al,0x31
	pop ax
	jz changeval0
	
	push ax
	add di,154
	mov al,byte[es:di]
	sub di,154
	cmp al,0x31
	pop ax
	jz changeval0
	jnz exit1
	;number checks for shape1(L) End
	
changeval0:
	mov word[bp+4],0
exit1:
	mov sp,bp
	pop bp
	ret

shape1Left:
	push bp
	mov bp,sp
	;shape1(L) and shape2(I) Checks start
	push ax
	mov al,byte[ds:si]
	cmp al,0x31
	pop ax
	jz shl1
	
	push ax
	sub si,160
	mov al,byte[ds:si]
	add si,160
	cmp al,0x31
	pop ax
	jz shl1
	
	push ax
	sub si,320
	mov al,byte[ds:si]
	add si,320
	cmp al,0x31
	pop ax
	jz shl1
	jnz shl1exit
	;shape1(L) and shape2(I)number check ENDS
shl1:
	mov word[bp+4],0
shl1exit:
	mov sp,bp
	pop bp
	ret
	
shape1Right:
	;shape1(L) number check
	push bp
	mov bp,sp
	push ax
	add di,2
	mov al,byte[es:di]
	sub di,2
	cmp al,0x31
	pop ax
	jz sh1r
	
	push ax
	sub di,4
	sub di,160
	mov al,byte[es:di]
	add di,164
	cmp al,0x31
	pop ax
	jz sh1r

	push ax
	sub di,324
	mov al,byte[es:di]
	add di,324
	cmp al,0x31
	pop ax
	jz sh1r
	jnz sh1rexit
	;shape1(L) number check ENDS
sh1r:
	mov word[bp+4],0
	
sh1rexit:
	mov sp,bp
	pop bp
	ret
	
Moving:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push ds
	
	mov word[tempds],ds
	
	mov ax,0xb800
	mov es ,ax
	mov ds,ax
	
	;mov dx,0
	;mov bx,1	;this is the row # for back row printing
	mov cx,100
	push cx 
	;**********************
	;generate random number for the first shape
	
	; call RANDNUM
	mov word[randNum],1
genran:
	;call RANDNUM
	mov bx, [randNum]
	mov word[currentShape],bx
	
	push bx
	
	sub sp, 2
	push 3
	call randG
	pop bx
	inc bx
	
	mov word[randNum],bx
	pop bx
	
	call UpcomingShape
	;mov word[randNum],bx
	
		
	; call RANDNUM
	; call UpcomingShape
	; mov bx, [currentShape]
	;**********************
	;call delay
	
	cmp bx,1
	jz firstshape1
	cmp bx,2
	jz firstshape2
	cmp bx,3
	jz firstshape3
	cmp bx,4
	jz firstshape4
	
firstshape1:
	mov ax, 2	;rows for upcoming shape
	;mov ax, [rcor]
	push ax
	mov dx,20	;cols for upcoming Shape
	;mov dx, [ccor]
	push dx
	push 0x2231
	;cmp random no with 0 if yes then call shape1
	call shape1
	
	;;;;
	push ax
	sub sp,2
	call shape1Down
	pop ax
	cmp ax,0
	pop ax
	jz MovingEnd
	;;;;
	
	jmp again
firstshape2:
	mov ax, 2	;rows for upcoming shape
	; mov ax, [rcor]
	push ax
	mov dx,20	;cols for upcoming Shape
	; mov dx, [ccor]
	push dx
	push 0x3331
	;cmp random no with 0 if yes then call shape1
	call shape2
	
	;;;;
	push ax
	sub sp,2
	call shape2Down
	pop ax
	cmp ax,0
	pop ax
	jz MovingEnd
	;;;;
	jmp again
firstshape3:
	mov ax, 2	;rows for upcoming shape
	; mov ax, [rcor]
	push ax
	mov dx,20	;cols for upcoming Shape
	; mov dx, [ccor]
	push dx
	push 0x4431
	;cmp random no with 0 if yes then call shape1
	call shape3
	
	;;;;
	push ax
	sub sp,2
	call shape3Down
	pop ax
	cmp ax,0
	pop ax
	jz MovingEnd
	;;;;
	
	jmp again
firstshape4:
	mov ax, 2	;rows for upcoming shape
	; mov ax, [rcor]
	push ax
	mov dx,20	;cols for upcoming Shape
	; mov dx, [ccor]
	push dx
	push 0x5531
	;cmp random no with 0 if yes then call shape1
	call shape4	
	
	;;;;
	push ax
	sub sp,2
	call shape4Down
	pop ax
	cmp ax,0
	pop ax
	jz MovingEnd
	;;;;
	
	jmp again
	
again:
	push ax
	push dx
	mov al,0h
	int 16h
	cmp al,0x73
	mov cl,al
	pop dx
	pop ax
	jz down
	jnz cmpLeft
down:
	;Check for bottom border
	push ax
	add di,160
	mov al,byte[es:di]
	sub di,160
	cmp al,0x2A
	pop ax
	;jz SubMovingEnd
	; jz subl1
	jz Ml2
	
	cmp bx,1
	jz downShape1
	cmp bx,2
	jz downShape2
	cmp bx,3
	jz downShape3
	cmp bx,4
	jz downShape4
;cmp random no with 0 if yes then call downShape1	
downShape1:
	push ax
	sub sp,2
	call shape1Down
	pop ax
	cmp ax,0
	pop ax
	jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape1
	
	add ax,1
	push ax
	push dx
	push 0x2231
	call shape1
	jmp Ml1
	
;cmp random no with 1 if yes then call shape2
downShape2:
	push ax
	sub sp,2
	call shape2Down
	pop ax
	cmp ax,0
	pop ax
	; jz subl1
	jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape2
	
	add ax,1
	push ax
	push dx
	push 0x3331
	call shape2
	jmp Ml1
	
downShape3:
	push ax
	sub sp,2
	call shape3Down
	pop ax
	cmp ax,0
	pop ax
	; jz subl1
	jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape3
	
	add ax,1
	push ax
	push dx
	push 0x4431
	call shape3
	jmp Ml1
	
downShape4:
	push ax
	sub sp,2
	call shape4Down
	pop ax
	cmp ax,0
	pop ax
	; jz subl1
	jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape4
	
	add ax,1
	push ax
	push dx
	push 0x5531
	call shape4
	jmp Ml1
	
SubMovingEnd:
	jmp MovingEnd
subl1:
	jmp Ml1
	
cmpLeft:
	push ax
	push dx
	cmp	cl,0x61
	pop dx
	pop ax
	jz Left
	jnz cmpRight
	
Left:
	;check for boundary of asterik
	push ax
	mov al,byte[ds:si]
	cmp al,0x2A
	pop ax
	jz subl1
	;jz Ml2
	
	cmp bx,1
	jz leftShape1
	cmp bx,2
	jz leftShape2
	cmp bx,3
	jz leftShape3
	cmp bx,4
	jz leftShape4
	
leftShape1:
	push ax
	sub sp,2
	call shape1Left
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	;jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape1
	
	push ax		;rows
	sub dx,1
	push dx		;cols
	push 0x2231
	call shape1
	jmp Ml1

leftShape2:
	push ax
	sub sp,2
	call shape2Left
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	;jz Ml2
	;printing of the respective shape
	push ax
	push dx
	push 0x7020
	call shape2
	
	push ax		;rows
	sub dx,1
	push dx		;cols
	push 0x3331
	call shape2
	jmp Ml1
	
leftShape3:
	push ax
	sub sp,2
	call shape3Left
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	;jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape3
	
	push ax		;rows
	sub dx,1
	push dx		;cols
	push 0x4431
	call shape3
	jmp Ml1
	
leftShape4:
	push ax
	sub sp,2
	call shape4Left
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	;jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape4
	
	push ax		;rows
	sub dx,1
	push dx		;cols
	push 0x5531
	call shape4
	jmp Ml1
	
cmpRight:
	push ax
	push dx
	cmp cl,0x64
	pop dx
	pop ax
	jz Right
	jnz Ml1	

Right:
	;check for boundary of asterik
	push ax
	add di,2
	mov al,byte[es:di]
	sub di,2
	cmp al,0x2A
	pop ax
	jz Ml1
	;jz Ml2
	
	cmp bx,1
	jz rightShape1
	cmp bx,2
	jz rightShape2
	cmp bx,3
	jz rightShape3
	cmp bx,4
	jz rightShape4
	
rightShape1:	
	push ax
	sub sp,2
	call shape1Right
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	; jz Ml2
	
	
	push ax
	push dx
	push 0x7020
	call shape1
	
	push ax		;rows
	add dx,1
	push dx		;cols
	push 0x2231
	call shape1
	jmp Ml1
	
rightShape2:	
	push ax
	sub sp,2
	call shape2Right
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	; jz Ml2
	
	push ax
	push dx
	push 0x7020
	call shape2
	
	push ax		;rows
	add dx,1
	push dx		;cols
	push 0x3331
	call shape2
	jmp Ml1
	
rightShape3:
	push ax
	sub sp,2
	call shape3Right
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	
	push ax
	push dx
	push 0x7020
	call shape3

	push ax		;rows
	add dx,1
	push dx		;cols
	push 0x4431
	call shape3
	jmp Ml1
	
rightShape4:
	push ax
	sub sp,2
	call shape4Right
	pop ax
	cmp ax,0
	pop ax
	jz Ml1
	
	push ax
	push dx
	push 0x7020
	call shape4

	push ax		;rows
	add dx,1
	push dx		;cols
	push 0x5531
	call shape4
	jmp Ml1
	
Ml2:
call Sound
	;1212
	mov ds,[tempds]
	;1212
	call ScanTotalGamePannel
	;121212
	push ax
	mov ax,0xb800
	mov ds,ax
	pop ax
	;12121
	pop cx
	dec cx
	push cx
	cmp cx,0
	jz MovingEnd
	jmp genran
	
Ml1:
	; pop cx
	; dec cx
	; push cx
	;cmp cx,0
	;jnz again	
	jmp again
	
	
	
MovingEnd:

	; mov word[cs:sec1],2

	pop ds
	pop bx
	pop di
	pop si
	pop cx
	pop ax
	pop es
	mov sp,bp
	pop bp
	ret

;bin zain codes
ScanTotalGamePannel:

	push ax 
	push bx
	push cx
	
	mov cx , 21 ; last row 
	
	lap:
		push cx 
		call ScanRow
		dec cx
		cmp cx,2
	jne lap
	
	
	
	pop cx
	pop bx 
	pop ax
	ret

ScanRow:		
				; subroutine by mbz 
				 
	push bp		; it will take row number as a paramter [bp +4]
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push es 
	push di
		
	ScanRowStart:
	mov al, 80 ; load al with columns per row 
	mul byte[bp+4] ;  
	add ax, 2 ;
	shl ax, 1 
	mov di,ax
	mov dl,al
		
	mov ax ,0xb800
	mov es, ax
	mov ax ,0x7020
	;mov cx ,40
	mov cx ,44
	cld
	
	repne scasw
	cmp cx,0
	jnz ScanRowEnd		; it will jump if space if there in the the row given of the vedio memory 
	mov ax ,[bp+4]
	push ax 
	call DeleteRow
	jmp ScanRowStart
	
ScanRowEnd:	
	pop di 
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
ret 2

DeleteRow:				; subrotine by mbz 
						; this function takes the row number of game pannel which is to be deleted and the upper rows of that pannal will shift down

	push bp ; it will take row number as a paramter [bp +4]
	mov bp,sp		; 
	push ax 
	push bx 
	push cx 
	push dx 
	push es 
	push di 
	push si
	push ds

	mov ax ,80
	mul byte[bp+4]
	mov di , ax
	add di , 4 
	shl di,1
	mov si, di
	sub si,160
	mov ax, 0xb800
	mov es,ax
	mov ds,ax
	mov dx ,19;dx mai number of rows to neechai lany rakha hy 
	mov ax , 21	
	sub ax,[bp+4]
	sub dx,ax		; in calculations k baad desire row sirf ho gi 
abc:
	mov ax , si
	push ax
	mov cx ,40
	cld ; in auto increment mode 
	rep movsw ; will shift only one row
	pop di
	; mov di,ax
	mov si,di
	sub si, 160
	dec dx 
	jnz abc


	; neutralizing first row of game pannel 
	mov ax,0x7020
	mov cx ,40
	mov di ,328
	cld
	rep stosw
	
	; updating score 
		
	pop ds ; data segment valy masail sy bachny k lyai :_(	
	mov ax , [score]
	add ax , 10
	mov word[score],ax
	
	call PrintScoreNum
	

	;pop ds ; data segment valy masail sy bachny k lyai :_(
	pop si 
	pop di 
	pop es 
	pop dx
	pop cx
	pop bx 
	pop ax
	pop bp
	ret 2
	
	
	
RANDNUM:
   push bp
   mov bp,sp
   push ax
   push cx
   push dx
   
   MOV AH, 00h  ; interrupts to get system time        
   INT 1AH      ; CX:DX now hold number of clock ticks since midnight      
   mov  ax, dx
   xor  dx, dx
   mov  cx, 3 
   inc cx   
   div  cx       ; here dx contains the remainder of the division - from 0 to 9
   inc dx
   mov word[randNum], dx
   
   pop dx
   pop cx
   pop ax
   pop bp   
   ret 

printnum: 

push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov ax, [bp+4] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigit ; if no divide it again 
 mov di, 140 ; point di to 70th column 
nextpos: pop dx ; remove a digit from the stack 
 mov dh, 0x07 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos ; repeat for all digits on stack 
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
pop es 
 pop bp 
 ret 2

PrintScoreNum:
	push es 
	push ax 
	push bx 
	push cx 
	push dx 
	push di 
	mov ax, 0xb800 
	mov es, ax ; point es to video base 
	mov ax, [score] ; load number in ax 
	mov bx, 10 ; use base 10 for division 
	mov cx, 0 ; initialize count of digits 
nextdigitScore:
	mov dx, 0 ; zero upper half of dividend 
	div bx ; divide by 10 
	add dl, 0x30 ; convert digit into ascii value 
	push dx ; save ascii value on stack 
	inc cx ; increment count of values 
	cmp ax, 0 ; is the quotient zero 
	jnz nextdigitScore ; if no divide it again 
	mov di, 2846 

nextposScore: 
	pop dx ; remove a digit from the stack 
	mov dh, 0x70 ; use normal attribute 
	mov [es:di], dx ; print char on screen 
	add di, 2 ; move to next screen location 
	loop nextposScore ; repeat for all digits on stack
	pop di 
	pop dx 
	pop cx 
	pop bx 
	pop ax 
	pop es 

	ret

page1:
push ax 
push bx

mov ax,25
push ax 

mov ax,6
push ax

mov ax , 10
push ax

mov ax,line1
push ax

call printstr

mov ax,34
push ax 

mov ax,7
push ax
mov ax , 140
push ax

mov ax,line2
push ax
call printstr

mov ax,20
push ax 

mov ax,9
push ax
mov ax , 14
push ax

mov ax,line3
push ax
call printstr

mov ax,35
push ax 

mov ax,11
push ax
mov ax , 0xE0
push ax

mov ax,line4
push ax
call printstr
pop bx
pop ax
ret 

page2:
	push ax 
	push bx
	push es 
	
	
	mov ax,6
	push ax 

	mov ax,6
	push ax

	mov ax , 0x87
	push ax

	mov ax,page21
	push ax
	call printstr
	
	mov ax,6
	push ax 

	mov ax,8
	push ax

	mov ax , 0x07
	push ax

	mov ax,page22
	push ax

	call printstr


	mov ax,6
	push ax 

	mov ax,10
	push ax

	mov ax , 0x07
	push ax

	mov ax,page23
	push ax

	call printstr
	
mov ax,37
push ax 

mov ax,13
push ax
mov ax , 0xE0
push ax

mov ax,line4
push ax
call printstr


	pop es
	pop bx
	pop ax
ret

intro:

call clrscr
 call page1
 mov ah,0 
int 16h 
call clrscr
call animation
call clrscr
 call page2
mov ah,0
 int 16h

ret




timer:
    push ax
    inc word [cs:tickcount] ; increment tick count
    cmp word [cs:tickcount], 18
    jnz l22
l11:
    dec word [cs:sec1]
    mov word[cs:tickcount],0
    cmp word [cs:sec1], 0
    jnz l22 ; Jump to end if seconds reach zero
l33:
    mov word [cs:gameover],1
    jmp endgame
    
l22:
	cmp word [cs:sec1],1000
	ja endtimer
    push word [cs:sec1]
    call timerprintnum; Print remaining seconds
endtimer:
    mov al, 0x20
    out 0x20, al ; End of interrupt
    pop ax
    iret ; Return



timerprintnum:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov ax, [bp+4] ; load number in ax
    mov bx, 10 ; use base 10 for division
    mov cx, 0 ; initialize count of digits

nextdigittimer:
    mov dx, 0 ; zero upper half of dividend
    div bx ; divide by 10
    add dl, 0x30 ; convert digit into ascii value
    push dx ; save ascii value on stack
    inc cx ; increment count of values
    cmp ax, 0 ; is the quotient zero
    jnz nextdigittimer ; if no divide it again

    mov di, 2526  ; balabalalalalalal point di to 79th column
nextpostimer:
    pop dx ; remove a digit from the stack
    mov dh, 0x70 ; use normal attribute
    mov [es:di], dx ; print char on screen
    add di, 2 ; move to previous screen location
    loop nextpostimer ; repeat for all digits on stack
	mov word[es:di], 0x7000
	add di, 2
	mov word[es:di], 0x7000

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 2



shortdelay:

push ax 
push cx

mov ax , 500
shp1:
	mov cx ,250
	shp2:
	loop shp2
	dec ax
jnz shp1
	


pop cx
pop ax
ret

animation:
	push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

	
	mov ax , 0xb800
	mov es,ax
	
	mov cx,79
	mov si , 2
	mov ax , 0
	pgc:
	mov word[es:si],0x7020
	sub si,2
	mov word[es:si],0x0020
	add si,2
	
	add si , 60
	mov word[es:si],0x7020
	sub si,2
	mov word[es:si],0x0020
	add si,2
	
	
	add si , 60
	mov word[es:si],0x7020
	sub si,2
	mov word[es:si],0x0020
	add si,2
	
	
	add si,2
	call shortdelay
	
	loop pgc

	pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
ret

Sound:

    push ax
    push bx
    push cx
    push dx


    mov     al, 182         ; Prepare the speaker for the
    out     43h, al         ;  note.
    mov     ax, 9121        ; Frequency number (in decimal)
    ;  for middle C.
    out     42h, al         ; Output low byte.
    mov     al, ah          ; Output high byte.
    out     42h, al
    in      al, 61h         ; Turn on note (get value from
    ;  port 61h).
    or      al, 00000011b   ; Set bits 1 and 0.
    out     61h, al         ; Send new value.
    mov     bx, 25          ; Pause for duration of note.
    .pause1:
    mov     cx, 6535
    .pause2:
    dec     cx
    jne     .pause2
    dec     bx
	jne     .pause1
    in      al, 61h         ; Turn off note (get value from
    ;  port 61h).
    and     al, 11111100b   ; Reset bits 1 and 0.
    out     61h, al         ; Send new value.

    pop dx
    pop cx
    pop bx
    pop ax

    ret

start:

	push ds
	call intro
	call clrscr
	mov ax, 0xE7    
	push ax
	call colorscr    ;this has been modified at 6:45pm 11/21/2023 because we want to use the same screen in the end but with different color

	mov ax,2	;row for gray screen
	push ax
	mov ax,4	;col for gray screen
	push ax
	call print
	;call PrintAllShapes

	;bin zain main
	call PrintTime
	call PrintScore
	call PrintUpcomingShape
	call PrintScoreNum
	;call UpcomingShape
	
	;call delay
	
	
	xor ax, ax
    mov es, ax ; point es to IVT base 
    mov ax,[es:8*4]
    mov [oldisr],ax
    mov ax,[es:8*4+2]
    mov [oldisr+2],ax
    cli ; disable interrupts
    mov word [es:8*4], timer ; store offset at n*4
    mov [es:8*4+2], cs ; store segment at n*4+2
    sti ; enable interrupts
	
	
	call Moving
endgame:
	
	; mov ax,[oldisr]
    ; mov bx,[oldisr+2]
    ; cli
    ; mov [es:8*4],ax
    ; mov [es:8*4+2],bx
    ; sti
	
	;mov word[sec1],2004
	mov word[cs:sec1],2004
	;call clrscr
	call animation
	call delay
	mov ax, 0x55
	push ax
	call colorscr
	
	pop ds
	call PrintEnd
	; call PrintYourScore
	; call PrintScoreNumEnd
	
	mov ax,[oldisr]
    mov bx,[oldisr+2]
    cli
    mov [es:8*4],ax
    mov [es:8*4+2],bx
    sti


end:
	mov ax,0x4c00
	int 21h


; PrintScoreNumEnd:
	; push es 
	; push ax 
	; push bx 
	; push cx 
	; push dx 
	; push di 
	; mov ax, 0xb800 
	; mov es, ax ; point es to video base 
	; mov ax, [score] ; load number in ax 
	; mov bx, 10 ; use base 10 for division 
	; mov cx, 0 ; initialize count of digits 
; nextdigitScoreEnd:
	; mov dx, 0 ; zero upper half of dividend 
	; div bx ; divide by 10 
	; add dl, 0x30 ; convert digit into ascii value 
	; push dx ; save ascii value on stack 
	; inc cx ; increment count of values 
	; cmp ax, 0 ; is the quotient zero 
	; jnz nextdigitScoreEnd ; if no divide it again 
	; mov di, 2006

; nextposScoreEnd: 
	; pop dx ; remove a digit from the stack 
	; mov dh, 0x70 ; use normal attribute 
	; mov [es:di], dx ; print char on screen 
	; add di, 2 ; move to next screen location 
	; loop nextposScoreEnd ;repeat for all digits on stack
	; pop di 
	; pop dx 
	; pop cx 
	; pop bx 
	; pop ax 
	; pop es 

	; ret
	