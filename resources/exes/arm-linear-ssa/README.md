# ARM Linear SSA

"Linear SSA" is a particular form of SSA used by the `Bap_vibes.Ir` module.
In effect, it requires that if a variable (e.g., `R0`) occurs in more
than one block, it gets a unique name in each block. 

This example has a `main` function that has multiple blocks, which VIBES
can put into Linear SSA form. In principle, if VIBES fails to transform
the program into a correct Linear SSA form, VIBES will choke on this example.

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
