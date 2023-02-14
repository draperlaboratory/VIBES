################################################
# 
#    Return 5
#
# To compile this:
#
#    powerpc-linux-gnu-gcc main.s -o main
#

# ----------------------------------------------
    .text

# ----------------------------------------------
# The MAIN function
# ----------------------------------------------
    .global main
    .type main, %function

main:
    subi 1, 1, 8
    li 3, 5
    addi 1, 1, 8
    blr
    .size main, .-main
