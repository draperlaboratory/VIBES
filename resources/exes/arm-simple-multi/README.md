# Simple Multi

A simple program that can be used for demonstrating/testing the VIBES tool.
This example demonstrates the application of multiple patch fragments.  The
return values of the functions "f1" and "f2" are changed.  This causes main,
which calls them, to take a different branch and return 1 rather than 0.

To build and patch:

    make

To just build (no patch):

    make build

To patch:

    make patch

To run in qemu:

    make test
