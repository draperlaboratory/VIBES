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
    mov r0, #5
    bx lr
    .size main, .-main
