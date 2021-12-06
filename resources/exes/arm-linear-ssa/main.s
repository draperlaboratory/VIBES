.section .text

.global main
.type main, %function
 
double_it:
    mov     r4, r0
    add     r4, r4
    mov     r0, r4
    bx      lr

main:
    mov     r0, #2
    bl      double_it
    bx      lr
    .size main, .-main
