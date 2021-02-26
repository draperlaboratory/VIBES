# Simple Compiled

A simple program that can be used for demonstrating/testing the VIBES tool.
This is meant to mirror the behavior of the "simple" example, but is compiled
rather than hand written in assembly.  This program consists of a `main`
function, which returns `5`.

To build and patch:

    make

To just build (no patch):

    make build

To patch:

    make patch

To run in qemu:

    make test
