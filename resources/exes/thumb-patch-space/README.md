# Thumb patch space

## Description

This test case is intended to exercise the `patch-space` feature, where users
can tell us about known regions in the binary that we can replace with patch
code (as opposed to hoping the patch fits in place or relying on the compiled-in
`vibes_dummy` region).

In the example, we replace a function call to a `g(x)`, which computes `x + 10`,
with some more complicated math - computing `x * 5 + 10` instead.


## Instructions

To clean, build, and patch:

    make

To build:

    make build

To patch:

    make patch

To run the executable and the patched executable (in qemu):

    make run.test

To create reference versions of the executables:

    make reference

To patch the reference executable:

    make patch.reference

To clean:

    make clean
