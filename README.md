# VIBES

This is the main external VIBES (DARPA AMP TA2) repository.

The VIBES tool is comprised of two components:

* An OCaml library.
* A command-line tool, which serves a front-end to the library. 

The library is called `bap-vibes`. Its source code lives in (lib/)[./lib]. 
The command-line tool is implemented as a BAP command, `bap vibes`. Its 
source code lives in (plugin/)[./plugin].


## Getting the code

Clone this repo wherever you prefer to keep your projects, for example:

    cd ~/code
    git clone https://github.com/draperlaboratory/VIBES.git

Then `cd` into the project root:

    cd VIBES


## Getting a development environment locally

Install `arm-linux-gnueabi-as`, `arm-linux-gnueabi-objcopy`, and
`arm-linux-gnueabi-gcc`. For example, on Ubuntu:

    apt install -y binutils-arm-linux-gnueabi gcc-arm-linux-gnueabi

If you don't have a `4.09.1` OCaml switch, create one.

Install the latest (bleeding edge) version of BAP:

    opam repo add bap-testing git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing
    opam depext --install -y bap

Clone [cbat_tools](https://github.com/draperlaboratory/cbat_tools), `cd`
into the `wp/lib/bap_wp` folder, and install with `make`:

    make

Then install the remaining dependencies needed for VIBES:

    opam install ounit2 ppx_deriving_yojson


## Installing/uninstalling

To install the tool (the library and the command-line front-end), `cd` into
the root of the project:

    cd VIBES

Then run `make`:

    make

To uninstall:

    make uninstall


## Just the library

If you want just the library (without the command-line front-end), follow
the instructions in [lib/README.md](./lib/README.md).


## Usage

The basic form of the command is this:

    bap vibes /path/to/pre-patched/exe [PARAMS] [OPTIONS]

This tells the VIBES tool to patch the EXE located on your system at
`/path/to/pre-patched/exe`.

A number of PARAMS must be specified:

* `--patch=NAME`:
  Specifies the name of a hand-written patch to apply to the EXE.
  Currently, there are two hand-written patches, named `ret-3` and `ret-4`.
  The first patch returns 3, and the second returns 4.
* `--patch-point=HEX`:
  Specifies the address in the EXE to start patching. Must be a valid hex
  string, e.g., `0x54`.
* `--patch-size=INT`:
  Specifies the number of bytes to overwrite with the patch.
* `--property=S-EXP`:
  Specifies the correctness property (as an S-expression) that VIBES should
  use to verify the correctness of the patched EXE.

The following are optional parameters:

* `--max-tries=INT`:
  Specifies the number of times to let VIBES try patching. Leaving this
  parameter unspecified or setting it to `0` tells VIBES to try forever.
* `--verbose`:
  If present, the VIBES tool will run with verbose logging. By default, it
  will log messages to `stderr` with TTY colors. You can turn the colors off
  by adding a `--no-colors` flag (see below).
* `--no-colors`:
  If present, this flag turns off colors in the verbose log. This is useful
  if you are piping the log to a file.

When the VIBES tool finishes patching the EXE, it will print the
filename (path) to the patched EXE to `stdout`. If it encounters an error,
it will exit and print the error to `stderr`.

Here is an example of invoking the command:

    bap vibes resources/simple/main \
        --patch=ret-3 \
        --patch-point=0x54 \
        --patch-size=8 \
        --property="(true)" \
        --verbose

This tells the VIBES tool to patch `resources/simple/main` with the
hand-written patch named `ret-3`. The patch should be inserted starting at
address `0x54` in `resources/simple/main`, and `8` bytes should be replaced.
The correctness properties to check is `true`. Finally, use verbose logging,
so that you can see the progress.
