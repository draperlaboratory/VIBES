@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ 
@    Return 5
@
@ To compile this:
@
@    arm-linux-gnueabi-gcc -marm main.s -o main
@

@ ----------------------------------------------
    .text

@ ----------------------------------------------
@ The MAIN function
@ ----------------------------------------------
    .global main
    .type main, %function

main:
    @ hack to enforce that fp is a valid memory location
    mov fp, sp
    mov r0, #5
    .rept 32
     nop
    .endr
    bx lr
    .size main, .-main
