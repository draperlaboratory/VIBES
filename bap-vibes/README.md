# The `bap-vibes` library

## Prerequisites

Before installing this library, follow the instructions under
[Installing the dependencies](../README.md#installing-the-dependencies)
to make sure you have the correct dependencies installed on your system.


## Building, installing, and testing

To clean and build the `bap-vibes` library, `cd` into this directory
and run `make`:

    cd <project-root>/lib
    make

Or, to build it (without cleaning first):

    make build

To run all tests (unit and integration):

    make test

To run just unit or integration:

    make test.unit
    make test.integration

To install the library into your local opam repository:

    make install

To uninstall it from your local opam repository:

    make uninstall

To clean the local build artifacts:

    make clean


