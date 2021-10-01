# ARM Stripped Loader

Tests the "vibes-raw" loader functionality, in which the bytes to
disassemble are specified in the `config.json` file, and where the
function symbols are stripped (and so the user has to specify their
name and location).


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
