.section .text

.global main
.type main, %function

.global double_it
.type double_it, %function

.global triple_it
.type triple_it, %function
 
double_it:
    push    {fp}
    mov     r3, r0
    add     r3, r3, r3
    mov     r0, r3
    pop     {fp}
    bx      lr
    .size double_it, .-double_it

triple_it:
    push    {fp}
    mov     r3, r0
    add     r3, r3, r3
    add     r3, r3, r0
    mov     r0, r3
    pop     {fp}
    bx      lr
    .size triple_it, .-triple_it

main:
    push    {fp, lr}
    mov     r0, #2
    bl      double_it
    mov     r3, r0
    add     r3, r3, #1
    mov     r0, r3
    bl      triple_it
    pop     {fp, pc}
    bx      lr
    .size main, .-main
