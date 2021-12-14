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
    .global vibes_dummy
    .type vibes_dummy, %function
vibes_dummy:
.rept 128
    nop
.endr
@ ----------------------------------------------
@ The MAIN function
@ ----------------------------------------------
    .global main
    .type main, %function
main:
    mov r0, #3
    mov r0, #4
    mov r0, #5
    bx lr
    .size main, .-main

