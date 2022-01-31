# Simple Branch

A simple program that can be used for demonstrating/testing the VIBES tool.
This function changes a check from a signed int to an unsigned int.
The modified function `foo` should return error code 2 and the original return error
code 1 on an input of -1.

To build and patch:

    make

To just build (no patch):

    make build

To patch:

    make patch

To run in qemu:

    make test
