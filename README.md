# VIBES

This is the main external VIBES (DARPA AMP TA2) repository.

The VIBES tool is comprised of two components:

* An OCaml library.
* A command-line tool, which serves a front-end to the library. 

The library is called `bap-vibes`. Its source code lives in [lib/](./lib). 
The command-line tool is implemented as a BAP command, `bap vibes`. Its 
source code lives in [plugin/](./plugin).


## Getting the code

Clone this repo wherever you prefer to keep your projects, for example:

    cd ~/code
    git clone https://github.com/draperlaboratory/VIBES.git

Then `cd` into the project root:

    cd VIBES

Copy the default minizinc model file into your home directory:

    mkdir -p ~/.vibes
    cp resources/minizinc/model.mzn ~/.vibes/


## Install the dependencies

Install `arm-linux-gnueabi-as`, `arm-linux-gnueabi-objcopy`, and
`arm-linux-gnueabi-gcc`. For example, on Ubuntu:

    apt install -y qemu binutils-arm-linux-gnueabi gcc-arm-linux-gnueabi

Install `minizinc`, for example if you use `snap`:

    snap install minizinc --classic

Or install it this way:

    cd ~
    curl -L "https://github.com/MiniZinc/MiniZincIDE/releases/download/2.5.3/MiniZincIDE-2.5.3-bundle-linux-x86_64.tgz" --output minizinc.tgz
    tar zxvf minizinc.tgz
    export PATH="${HOME}/MiniZincIDE-2.5.3-bundle-linux-x86_64/bin":"${PATH}"
    export LD_LIBRARY_PATH="${HOME}/MiniZincIDE-2.5.3-bundle-linux-x86_64/lib":"${LD_LIBRARY_PATH}"

If you don't have a `4.09.1` OCaml switch, create one. 

Install the latest (bleeding edge) version of BAP:

    opam repo add bap-testing git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing
    opam depext --install -y bap

Clone [cbat_tools](https://github.com/draperlaboratory/cbat_tools), `cd`
into the `wp/lib/bap_wp` folder, and install with `make`:

    make


## Installing/uninstalling

To install the tool (the library and the command-line front-end), `cd` into
the root of the project:

    cd VIBES

Then run `make`:

    make

To uninstall:

    make uninstall

To run all tests:

    make test


## Just the library

If you want just the library (without the command-line front-end), follow
the instructions in [bap-vibes/README.md](./bap-vibes/README.md).


## Usage

The basic form of the command is this:

    bap vibes /path/to/pre-patched/exe [PARAMS] [OPTIONS]

This tells the VIBES tool to patch the EXE located on your system at
`/path/to/pre-patched/exe`.

There is one mandatory PARAM:

* `--config=PATH`:
  Specifies the location of the configuration JSON file.  The format for this
  file, which provides the details of the patch, is provided below.

The following are optional parameters:

* `-o PATH`, `--output=PATH`:
  The output location for the patched binary.
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
        --config resources/simple/config.json \
        --verbose

This tells the VIBES tool to patch `resources/simple/main` with the patch
information provided in the `resources/simple/config.json` file and to use
verbose logging, so that you can see the progress.


## Configuration file format

The mandatory configuation file provides the details of the patch and several
optional parameters.  It is a JSON file with a single top level object.

The top-level object must include the following fields:

* `"func" : "NAME"` - 
  Specifies the name of the function you want to verify.
* `"property" : "S-EXP"` -
  Specifies the correctness property (as an S-expression in a JSON string)
  that VIBES should use to verify the correctness of FUNC in the patched exe.
* `"patches" : [PATCH-OBJECTS]"` -
  Specifies the patches to apply as an array of patch fragment description
  objects.  Each object in the array describes a change to a single contiguous
  piece of assembly code.  The objects have three fields:
  * `"patch-name" : "NAME"` -
    Currently, there are two hand-written patches, named `ret-3` and `ret-4`.
    The first patch returns 3, and the second returns 4.
  * `"patch-point" : "HEX"` -
    Specifies the address in the EXE to start patching.  Must be a valid hex
    number in a JSON string, e.g., `"0x54"`.
  * `"patch-size" : INT` -
    Specifies the number of bytes to overwrite with the patch.

The top-level object may include the following optional field:

* `"max-tries" : INT` -
  Specifies the number of times to let VIBES try patching. Leaving this
  parameter unspecified or setting it to `0` tells VIBES to try forever.
* `"minizinc-model" : FILEPATH` -
  Specifies the path to a minizinc model filepath. If this is omitted,
  VIBES will look for the file at `~/.vibes/model.mzn`.

Here is an example of a valid configuration file, taken from the
`resources/simple` example:

```
{
  "func": "main",
  "property" : "(assert true)",
  "patches" : [
    {"patch" : "ret-3",
     "patch-point" : "0x54",
     "patch-size" : 8}
  ]
}
```

This tells VIBES to use hand-written patch named `ret-3`. There is one patch,
which should be inserted starting at address `0x54` in `resources/simple/main`,
and `8` bytes should be replaced.  The correctness property to use to check 
the function `main` is `(assert true)`.
