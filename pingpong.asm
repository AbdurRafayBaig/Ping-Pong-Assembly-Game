; clear the screen
[org 0x0100]
  jmp start

;<===============================Initialize All Arrays===================================>
PingPong: db 'PING PONG GAME',0
P1name: db 'First player name and ID  : Abdur Rafay(23F-3046)',0
P2name: db 'Seconde player name and ID: Ammar Ahmad(23P-3071)',0
message: db 'Abdur Rafay(23F-3046):',0
message1: db 'Ammar Ahmad(23P-3071):',0
message2: db 'Abdur Rafay(23F-3046): Win the Game',0
message3: db 'Ammar Ahmad(23P-3071): Win the Game',0
message4: db 'Press any key to exit a game',0
message5: db 'Press any key to Enter a game',0
P1: dw 1600,1760,1920
P2: dw 1758,1918,2078  
player1:dw 0
player2:dw 0


;<===============================Clear Screen===================================>
clrscr: push es
        push ax
        push di

        mov ax, 0xb800
        mov es, ax          ; point es to video base
        mov di, 0           ; point di to top left column

nextloc: mov word [es:di], 0x0720        ; clear next char on screen
         add di, 2                       ; move to next screen location
         cmp di, 4000                    ; has the whole screen cleared
         jne nextloc                     ; if no clear next position

         pop di
         pop ax
         pop es
         ret

;////////////////////////////////////////////////////////////////////////////////////////
;<===============================Print All Strings=====================================>
;/////////////////////////////////////////////////////////////////////////////////////////

;<=====================Conunt Lenth of strings=========================>

strlen: push bp
        mov bp, sp
        push es
        push cx
        push di
        les di, [bp + 4]                     ; point es : di to string
        mov cx, 0xffff                       ; load maximum number in cx
        xor al, al                           ; load a zero in al
        repne scasb                          ; find zero in the string
        mov ax, 0xffff                       ; load maximum number in ax
        sub ax, cx                           ; find change in cx
        dec ax                               ; exclude null from length
        pop di
        pop cx
        pop es
        pop bp
        ret 4



;<=================================Ping Pong String===========================================>

PingPongPrint: push bp
               mov bp, sp
               push es
               push ax
               push cx
               push si
               push di
               push ds                          ; push segment of string
               mov ax, [bp + 4]
               push ax                          ; push offset of string
               call strlen                      ; calculate string length
    
               cmp ax, 0                        ; is the string empty
               jz Ping1                          ; no printing if string is empty
               mov cx, ax                       ; save length in cx
               mov ax, 0xb800
               mov es, ax                       ; point es to video base
               mov di, 1180                     ; point di to required location
               mov si, [bp + 4]                 ; point si to string
               mov ah, 0x02                     ; load attribute in ah

           cld                     ; auto increment mode
PingPong1: lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop PingPong1          ; repeat for the whole string

Ping1: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4

;<===================================First player Name=========================================>

player11: push bp
           mov bp, sp
           push es
           push ax
           push cx
           push si
           push di
           push ds                          ; push segment of string
           mov ax, [bp + 4]
           push ax                          ; push offset of string
           call strlen                      ; calculate string length

           cmp ax, 0                        ; is the string empty
           jz notP1                          ; no printing if string is empty
           mov cx, ax                       ; save length in cx
           mov ax, 0xb800
           mov es, ax                       ; point es to video base
           mov di, 1480                       ; point di to required location
           mov si, [bp + 4]                 ; point si to string
           mov ah, 0x04                     ; load attribute in ah

           cld                     ; auto increment mode
nextplayer1: lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop nextplayer1          ; repeat for the whole string

notP1: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4



;<=================================Seconde Player Name=======================================>

player22: push bp
           mov bp, sp
           push es
           push ax
           push cx
           push si
           push di
           push ds                          ; push segment of string
           mov ax, [bp + 4]
           push ax                          ; push offset of string
           call strlen                      ; calculate string length

           cmp ax, 0                        ; is the string empty
           jz notP2                          ; no printing if string is empty
           mov cx, ax                       ; save length in cx
           mov ax, 0xb800
           mov es, ax                       ; point es to video base
           mov di, 1640                       ; point di to required location
           mov si, [bp + 4]                 ; point si to string
           mov ah, 0x01                     ; load attribute in ah

           cld                     ; auto increment mode
nextplayer2: lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop nextplayer2          ; repeat for the whole string

notP2: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4



;<======================Press any key to Enter a game print=======================>

Ent1:    push bp
          mov bp, sp
          push es
          push ax
          push cx
          push si
          push di
          push ds                          ; push segment of string
          mov ax, [bp + 4]
          push ax                          ; push offset of string
          call strlen                      ; calculate string length
          cmp ax, 0                        ; is the string empty
          jz Ent                         ; no printing if string is empty
          mov cx, ax                       ; save length in cx
          mov ax, 0xb800
          mov es, ax                       ; point es to video base
          mov di, 1978                     ; point di to required location
          mov si, [bp + 4]                 ; point si to string
          mov ah, 0x02                     ; Green blinking text on black background


           cld                     ; auto increment mode
nextEnter: lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop nextEnter          ; repeat for the whole string

Ent:   pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4



;<==========================First player Name In Game===============================>

printstr1: push bp
           mov bp, sp
           push es
           push ax
           push cx
           push si
           push di
           push ds                          ; push segment of string
           mov ax, [bp + 4]
           push ax                          ; push offset of string
           call strlen                      ; calculate string length

           cmp ax, 0                        ; is the string empty
           jz exit1                          ; no printing if string is empty
           mov cx, ax                       ; save length in cx
           mov ax, 0xb800
           mov es, ax                       ; point es to video base
           mov di, 86                       ; point di to required location
           mov si, [bp + 4]                 ; point si to string
           mov ah, 0x01                     ; load attribute in ah

           cld                     ; auto increment mode
nextchar1: lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop nextchar1          ; repeat for the whole string

exit1: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4


;<======================Seconde player Name In Game=======================>

print1:    push bp
           mov bp, sp
           push es
           push ax
           push cx
           push si
           push di
           push ds                          ; push segment of string
           mov ax, [bp + 4]
           push ax                          ; push offset of string
           call strlen                      ; calculate string length

           cmp ax, 0                        ; is the string empty
           jz ex1                          ; no printing if string is empty
           mov cx, ax                       ; save length in cx
           mov ax, 0xb800
           mov es, ax                       ; point es to video base
           mov di, 12                       ; point di to required location
           mov si, [bp + 4]                 ; point si to string
           mov ah, 0x04                     ; load attribute in ah

           cld                     ; auto increment mode
next1:     lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop next1              ; repeat for the whole string

ex1: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4



;<=======================================First player Win Print==============================================>

First: push bp
       mov bp, sp
       push es
       push ax
       push cx
       push si
       push di
       push ds                          ; push segment of string
       mov ax, [bp + 4]
       push ax                          ; push offset of string
       call strlen                      ; calculate string length      
       cmp ax, 0                        ; is the string empty
       jz exit2                          ; no printing if string is empty
       mov cx, ax                       ; save length in cx
       mov ax, 0xb800
       mov es, ax                       ; point es to video base
       mov di, 1656                     ; point di to required location
       mov si, [bp + 4]                 ; point si to string
       mov ah, 0x04                     ; load attribute in ah

           cld                     ; auto increment mode
nextchar2: lodsb                  ; load next char in al
           stosw                   ; print char / attribute pair
           loop nextchar2          ; repeat for the whole string

exit2: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4


;<==========================Seconde Player Win==================================>

Seconde:  push bp
          mov bp, sp
          push es
          push ax
          push cx
          push si
          push di
          push ds                          ; push segment of string
          mov ax, [bp + 4]
          push ax                          ; push offset of string
          call strlen                      ; calculate string length
          cmp ax, 0                        ; is the string empty
          jz exit3                         ; no printing if string is empty
          mov cx, ax                       ; save length in cx
          mov ax, 0xb800
          mov es, ax                       ; point es to video base
          mov di, 1656                        ; point di to required location
          mov si, [bp + 4]                 ; point si to string
          mov ah, 1                        ; load attribute in ah

           cld                     ; auto increment mode
nextchar3: lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop nextchar3          ; repeat for the whole string

exit3: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4


;<======================Exit condition print=======================>

press:    push bp
          mov bp, sp
          push es
          push ax
          push cx
          push si
          push di
          push ds                          ; push segment of string
          mov ax, [bp + 4]
          push ax                          ; push offset of string
          call strlen                      ; calculate string length
          cmp ax, 0                        ; is the string empty
          jz exit4                         ; no printing if string is empty
          mov cx, ax                       ; save length in cx
          mov ax, 0xb800
          mov es, ax                       ; point es to video base
          mov di, 1978                     ; point di to required location
          mov si, [bp + 4]                 ; point si to string
          mov ah, 0x02                     ; Green blinking text on black background


           cld                     ; auto increment mode
nextchar4: lodsb                   ; load next char in al
           stosw                   ; print char / attribute pair
           loop nextchar4          ; repeat for the whole string

exit4: pop di
       pop si
       pop cx
       pop ax
       pop es
       pop bp
       ret 4


;///////////////////////////////////////////////////////////////////////////////////////////
;<===================================All Cnditions of Game==================================>
;////////////////////////////////////////////////////////////////////////////////////////////

;<=====================Border=========================>

Border: push es
        push ax
        push di

        mov ax, 0xb800
        mov es, ax          ; point es to video base
        mov di, 320           

;First Border
nextB: mov word [es:di], 0x2020       
       add di, 2                       
       cmp di, 480                    
       jne nextB

;Last Line Border
       mov di,3840
nextBL:mov word [es:di], 0x2020       
       add di, 2                       
       cmp di, 4000                    
       jne nextBL  
                       

         pop di
         pop ax
         pop es
         ret

;///////////////////////////////////////////////////////////////////////////////////////////
;<===================================All Cnditions of Padel==================================>
;////////////////////////////////////////////////////////////////////////////////////////////

;<===================Move 1st Padel(Using key of W)=======================>
moveP1:
       push ax
       push di
       mov si,0
move1: mov di,[P1+si]
       mov word [es:di], 0x0720
       mov ax,[P1+si]
       add ax,160
       mov [P1+si],ax
       add si,2
       cmp si,6
       jne move1
       call Padel

       pop di
       pop ax
       ret
       


;<===================Move 2nd Padel(Using key of Page Up)=======================>
moveP2:             
       push ax
       push di
       push es

       mov si,0
move2: mov di,[P2+si]
       mov word [es:di], 0x0720
       mov ax,[P2+si]
       add ax,160
       mov [P2+si],ax
       add si,2
       cmp si,6
       jne move2
       call Padel
   
       pop es
       pop di
       pop ax
       ret


;<===================Return Move 1st Padel(Using key of S)=======================>
returnmoveP1:
       push ax
       push di
       push es

       mov si,0
rmove1:mov di,[P1+si]
       mov word [es:di], 0x0720
       mov ax,[P1+si]
       sub ax,160
       mov [P1+si],ax
       add si,2
       cmp si,6
       jne rmove1
       call Padel

       pop es
       pop di
       pop ax
       ret
       


;<===================Return Move 2nd Padel(Using key of Page down)=======================>
returnmoveP2:
       push ax
       push di
       push es

       mov si,0
rmove2: mov di,[P2+si]
       mov word [es:di], 0x0720
       mov ax,[P2+si]
       sub ax,160
       mov [P2+si],ax
       add si,2
       cmp si,6
       jne rmove2
       call Padel

       pop es
       pop di
       pop ax
       ret

;<=====================Padels=========================>

Padel:  push es
        push ax
        push di
        push si
        push cx

        mov ax, 0xb800
        mov es, ax          ; point es to video base
        mov si,0          

;First Padel
nextP1:mov di, [P1+si]
       mov word [es:di], 0x4020
       add si,2                              
       cmp si,6                  
       jne nextP1



;Seconde Padel
       mov si,0
nextP2:mov di, [P2+si]
       mov word [es:di], 0x1020
       add si,2                          
       cmp si,6                    
       jne nextP2 
       

         pop si
         pop cx              
         pop di
         pop ax
         pop es
         ret

;<====================Delay===========================>

delay:
    push cx
    push ax


    mov ax,1
delayMaker:
    mov cx,0xFFFF
delayer:  
    loop delayer
    dec ax  
    cmp ax,-3

    jne delayMaker
    pop ax
    pop cx
    ret


;<=====================keyboard====================>

kbisr:
    push ax
    push es
    push di
    push si

    mov di,0
    mov si,0
    mov ax, 0xb800
    mov es, ax              ; Point ES to video memory
    in al, 0x60             ; Read a char from keyboard port
    cmp al, 0x1F            ; S key press for below move 1st Padel
    jne nextcmp             ; No, try next comparison
    mov di,[P1+si+4]
    cmp di,3680
    je nextcmp 
    call moveP1
    jmp nomatch             ; Leave interrupt routine

nextcmp:
    mov di,0
    mov si,0
    cmp al, 0x11             ; W key press for above move 1st Padel
    jne nextcmp1             ; No, leave interrupt routine
    mov di,[P1+si]
    cmp di,480
    je nextcmp1 
    call returnmoveP1
    jmp nomatch

nextcmp1:
    mov di,0
    mov si,0
    cmp al, 0x50             ; arrow key press for below move 2nd Padel
    jne nextcmp2             ; No, leave interrupt routine
    mov di,[P2+si+4]
    cmp di,3838
    je nextcmp2 
    call moveP2
    jmp nomatch

nextcmp2:
    mov di,0
    mov si,0
    cmp al, 0x48            ;arrow key press for above move 2nd Padel
    jne nomatch             ; No, leave interrupt routine
    mov di,[P2+si]
    cmp di,638
    je nomatch 
    call returnmoveP2

nomatch:
    mov al, 0x20
    out 0x20, al            ; Send EOI to PIC

    pop si
    pop di
    pop es
    pop ax
    iret

;////////////////////////////////////////////////////////////////////////
;<=======================All conditons of BaLL=======================>
;///////////////////////////////////////////////////////////////////////

;<=======================BaLL=======================>
Ball:   push es
        push ax
        push di
        push bx
        push dx
        push si
        push cx
        push bp

        mov word [es:di], 0x0720
        mov bx,3680
        mov cx,640
        mov ax, 0xb800
        mov es, ax          ; point es to video base
        mov di, 2000 
        mov word [es:di], 0x0730
        mov dx, 162

;<==============================MoveBall======================================>
      
 MoveBall: call delay
           mov word [es:di], 0x0720
           add di,dx
           mov word [es:di], 0x0730

           cmp di,bx             
           jg Bottomwall     ;When Bottom wall Find
           
           cmp di,cx             
           jl topwall        ;When Top wall Fint


;<=================When Pade One Find========================>
          mov si,0
          mov bp,0
P11:      mov bp,[P1+si]
          add bp,2
          cmp di,bp
          je Pad1
          add si,2
          cmp si,6
          jne P11

;<===============When Seconde Pade Find========================>
          mov si,0 
          mov bp,0
P22:      mov bp,[P2+si]
          sub bp,2
          cmp di,bp
          je Pad2
          add si,2
          cmp si,6
          jne P22

          push di
          call checkballPadelend
          jmp MoveBall

Bottomwall:add dx,-320               ;164-320=-156
           jmp MoveBall

topwall: add dx,320                  ;-156+320=164
         jmp MoveBall

Pad2:  add dx,-4                 ;-156-8=-164
       jmp MoveBall

Pad1:  add dx,4                  ;156+8 = 164
       jmp MoveBall

        
end:     pop bp
         pop cx
         pop si
         pop dx
         pop bx
         pop di
         pop ax
         pop es
         ret

;<==================================Left and Right Wall End==================================>

checkballPadelend:push bp
                  mov bp,sp
                  push si
                  push ax
                  push bx

                mov ax,0
                mov bx,0
checkballP1end: mov si,0
loop1:          cmp [bp+4],si
                je secondPlayeGoal
                add si,160
                cmp si,3840
                jne loop1

checkballP2end: mov si,158
loop2:          cmp [bp+4],si
                je FirstPlayerGaol
                add si,160
                cmp si,3838
                jne loop2
                jmp end1
       
secondPlayeGoal:  mov ax,1
                  mov bx,[player2]
                  add bx,ax
                  cmp bx,3
                  je SecondePlayerWin
                  mov [player2],bx
                  mov ax,[player2]
                  push ax
                  call printnum2        ;Print Seconde Player Goal
                  jmp Ball

FirstPlayerGaol:  mov ax,1
                  mov bx,[player1]
                  add bx,ax
                  cmp bx,3
                  je FirstPlayerWin
                  mov [player1],bx
                  mov ax,[player1]
                  push ax
                  call printnum1         ;;Print First Player Goal
                  jmp Ball

FirstPlayerWin: call clrscr
                mov ax,0
                mov ax, message2
                push ax              ; push address of message
                call First
                jmp pressed

SecondePlayerWin: call clrscr
                  mov ax,0
                  mov ax, message3
                  push ax  
                  call Seconde
                  jmp pressed


 end1:          pop bx
                pop ax
                pop bp
                pop si
                ret 2

;////////////////////////////////////////////////////////////////////////////////
;<==========================print both Players Goal ========================>
;////////////////////////////////////////////////////////////////////////////////

; subroutine to print a number at top left of screen
; takes the number to be printed as its parameter
printnum1: push bp
           mov bp, sp
           push es
           push ax
           push bx
           push cx
           push dx
           push di
           
           mov ax, 0xb800
           mov es, ax         ; point es to video base
           mov ax, [bp + 4]   ; load number in ax
           mov bx, 10         ; use base 10 for division
           mov cx, 0          ; initialize count of digits

nextdigit : mov dx, 0            ; zero upper half of dividend
            div bx               ; divide by 10
            add dl, 0x30         ; convert digit into ascii value
            push dx              ; save ascii value on stack
            inc cx               ; increment count of values
            cmp ax, 0            ; is the quotient zero
            jnz nextdigit        ; if no divide it again
            mov di, 56         

nextpos : pop dx                 ; remove a digit from the stack
          mov dh, 0x04           ; use normal attribute
          mov[es:di], dx         ; print char on screen
          add di, 2              ; move to next screen location
          loop nextpos           ; repeat for all digits on stack


          pop di
          pop dx
          pop cx
          pop bx
          pop ax
          pop es
          pop bp
          ret 2


;<=================print 2nd Number after addition==============>
; subroutine to print a number at top left of screen
; takes the number to be printed as its parameter
printnum2: push bp
           mov bp, sp
           push es
           push ax
           push bx
           push cx
           push dx
           push di
           
           mov ax, 0xb800
           mov es, ax         ; point es to video base
           mov ax, [bp + 4]   ; load number in ax
           mov bx, 10         ; use base 10 for division
           mov cx, 0          ; initialize count of digits

nextdigit2 : mov dx, 0            ; zero upper half of dividend
            div bx               ; divide by 10
            add dl, 0x30         ; convert digit into ascii value
            push dx              ; save ascii value on stack
            inc cx               ; increment count of values
            cmp ax, 0            ; is the quotient zero
            jnz nextdigit2        ; if no divide it again
            mov di, 128        

nextpos2: pop dx                 ; remove a digit from the stack
          mov dh, 0x01           ; use normal attribute
          mov[es:di], dx         ; print char on screen
          add di, 2              ; move to next screen location
          loop nextpos2           ; repeat for all digits on stack
          pop di
          pop dx
          pop cx
          pop bx
          pop ax
          pop es
          pop bp
          ret 2


start: call clrscr

;<=================First Page=====================>
       mov ax,PingPong
       push ax
       call PingPongPrint
      
       mov ax,P1name
       push ax
       call player11       ;call a function that print(First Player Name)

       mov ax,P2name
       push ax
       call player22       ;call a function that print(Seconde Player Name)


       mov ax, message5
       push ax              
       call Ent1            ; call a function that print(press any key)

       mov ah,0x1
       int 0x21

;<================Seconde Page when game start=================>       
       call clrscr

;<================Genrate Interupt=====================>
       xor ax, ax
       mov es, ax              ; Point ES to IVT base
       cli                     ; Disable interrupts
       mov word [es:9*4], kbisr ; Store offset at n*4
       mov [es:9*4+2], cs      ; Store segment at n*4+2
       sti                     ; Enable interrupts

       mov ax, message
       push ax              
       call print1        ;call a function that print(First Player Name in Game)


       mov ax, message1
       push ax              
       call printstr1        ;call a function that print(Seconde Player Name in Game)



       mov ax,[player1]
       push ax
       call printnum1
       mov ax,[player2]
       push ax

;<=================Start Game Functions==============>    

       call printnum2       ;Starting goal score is zero
       call Border
       call Padel
       call Ball
;<================Last Page to print Exit a Game==========>
pressed:mov ax, message4
        push ax              
        call press

        mov ah,0x1
        int 0x21

exit5:
 mov ax, 0x4c00              
 int 21h
