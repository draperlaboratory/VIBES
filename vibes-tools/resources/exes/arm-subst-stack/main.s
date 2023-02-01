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
    sub sp, sp, #8
    mov r0, #5
    add sp, sp, #8
    bx lr
    .size main, .-main
