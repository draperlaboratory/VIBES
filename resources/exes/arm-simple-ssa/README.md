# ARM Simple SSA

This example patch has more than one occurrence of the same variable. In principle, if VIBES does not convert the patch into SSA form, it will choke on this example.

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
