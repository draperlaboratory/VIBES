# VIBES

VIBES (Verified, Incremental Binary Editing with Synthesis) is a tool that
uses program synthesis and constraint programming techniques to compile a
source-level patch and insert it into a preexisting binary program.  VIBES
uses formal verification to prove that only the intended change is made.

This is the main external VIBES repository.

The VIBES tool comprises two components:

* An OCaml library.
* A command-line tool, which serves a front-end to the library.

The library is called `bap-vibes`. Its source code lives in [lib/](./lib).
The command-line tool is implemented as a BAP command, `bap vibes`. Its
source code lives in [plugin/](./plugin).


## Acknowledgements

This work is sponsored by DARPA / NAVWAR Contract N6600120C4018, as part of
the DARPA Assured Micro-Patching (AMP) program.  Its content does not
necessarily reflect the position or policy of the US Government and no
official endorsement should be inferred.


## Installation

VIBES has been developed primarily on Ubuntu 18 and 20.  The instructions
below assume you are on one of these operating systems.


### Install OCaml and BAP

If you are running in the latest `binaryanalysisplatform/bap:latest` docker
container, you may skip this section. OCaml and BAP are already installed.

Otherwise, if you don't have a `4.09.1` OCaml switch, create one.

Install the latest (bleeding edge) version of BAP:

    opam repo add bap-testing git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing
    opam depext --install -y bap

### Get the VIBES source code

Clone this repo wherever you prefer to keep your projects, for example:

    cd ~/code
    git clone https://github.com/draperlaboratory/VIBES.git
    cd VIBES


### Install dependencies with a script

If you have bash on Ubuntu, you can run a script to install the remaining
dependencies. This script will not overwrite any previous installations, so
if you want to install, say, `wp` or `minizinc` in a custom way, do that
before you run this script.

To run the script, from the root of the VIBES repo:

    bash bin/setup/ubuntu.bash
    source bin/setup/env.bash


### Install the dependencies manually

To install the dependencies manually, perform the following steps.

Install `arm-linux-gnueabi-as`, `arm-linux-gnueabi-objcopy`,
`arm-linux-gnueabi-gcc`, and `cmake`.  For example, on Ubuntu:

    apt install -y qemu binutils-arm-linux-gnueabi gcc-arm-linux-gnueabi cmake

Then, install `minizinc`. For example, if you use `snap`:

    snap install minizinc --classic

Or install it this way:

    cd ~
    curl -L "https://github.com/MiniZinc/MiniZincIDE/releases/download/2.5.3/MiniZincIDE-2.5.3-bundle-linux-x86_64.tgz" --output minizinc.tgz
    tar zxvf minizinc.tgz
    export PATH="${HOME}/MiniZincIDE-2.5.3-bundle-linux-x86_64/bin":"${PATH}"
    export LD_LIBRARY_PATH="${HOME}/MiniZincIDE-2.5.3-bundle-linux-x86_64/lib":"${LD_LIBRARY_PATH}"

Next, install boolector, e.g.:

    cd ~
    git clone https://github.com/boolector/boolector
    cd boolector
    ./contrib/setup-lingeling.sh
    ./contrib/setup-btor2tools.sh
    ./configure.sh && cd build && make
    cd bin
    export PATH=$(pwd):$PATH

Clone [cbat_tools](https://github.com/draperlaboratory/cbat_tools), `cd`
into the `wp` folder, checkout the branch `codyroux/user-fun-spec-compare`, and install with `make`:

    cd ~
    git clone https://github.com/draperlaboratory/cbat_tools
    cd cbat_tools/wp
    git checkout codyroux/user-fun-spec-compare
    make

Install the following opam packages:

    opam install ounit2 ppx_deriving_yojson


### Installing/uninstalling the VIBES tool

To install the tool (the library and the command-line front-end), `cd` into
the root of the project:

    cd VIBES

Then run `make`:

    make

To uninstall:

    make uninstall

To run all tests:

    make test


### Just the library

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


## Config files

A config file is a JSON file that consists of at least two pieces
of information:

```
{
  "patches": [<A LIST OF PATCHES TO APPLY TO THE BINARY>],
  "wp-params": {<A DICTIONARY OF PARAMETERS FOR THE WP VERIFIER>},
}
```

For the `patches`, a patch looks like this:

```
{
  ...
  "patches": [
    {
      "patch-name": "<UNIQUE NAME FOR THE PATCH>",
      "patch-point": "<HEX ADDRESS AT WHICH TO START REPLACING BYTES>",
      "patch-size": <NUM BYTES TO REPLACE>,
      "patch-code": "<C-CODE TO COMPILE AND INSERT AT THE PATCH SITE>",
      "patch-vars": [<STORAGE CLASS INFO FOR VARIABLES IN THE C-CODE>]
    }
  ]
  ...
}
```

For `wp-params`, a minimal set looks like this:

```
{
  ...
  "wp-params": {
    "func": "<NAME OF FUNCTION IN THE BINARY TO VERIFY>",
    "postcond": "<A POSTCONDITION TO VERIFY (AN SMTLIB2 EXPRESSION)>",
  }
}
```

As an example, here is a simple config file:

```
{
  "patches": [
    {
      "patch-name": "my-patch",
      "patch-point": "0x10400",
      "patch-size": "4",
      "patch-code": "int x;
                     x = 3;",
      "patch-vars": []
    }
  ],
  "wp-params": {
    "func": "main",
    "postcond": "(assert (= R0_orig R0_mod))"
  }
}
```

That config file tells VIBES to do the following:

1. Go to address `0x10400` in the binary.
2. Replace 4 bytes with `int x; x = 3;` (compiled to machine code via CEGIS).
3. Ensure that the specified postcondition holds (i.e., that `R0` in the
original binary and `R0` in the modified binary cannot hold different
values at the end of the function `main`, given the same inputs).


### Storage classification

In the `patch-vars` field of a patch, you can provide storage classification
for identifiers that appear in the patch code. For instance, suppose you have
a patch with the following patch code:

```
int x; 
x = 3;
```

To tell VIBES that `x` should be stored in the register `R0` at the entrance
to the patch site, and that `x` should be stored in the register `R1` at the
exit of the patch site, add an entry to the `patch-vars` list like this:

```
{
  "patches": [
    {
      ...
      "patch-code": "int x;
                     x = 3;",
      "patch-vars": [
        {
          "name": "x",
          "at-entry": {
            "stored-in": "register",
            "register": "R0"
          },
          "at-exit": {
            "stored-in": "register",
            "register": "R1"
          }
        },
        ...
      ]
    }
  ],
  ...
}
```

Alternatively, suppose you want to tell VIBES that, at the entrance to the
patch site, `x` should be in `R2`, while at the exit of the patch site, `x`
should live on the stack at `0x4:32` past the frame pointer (`0x4:32` is the
number `0x4` with a bitwidth of `32`). The entry to `patch-vars` would then
look like this:

```
{
  "patches": [
    {
      ...
      "patch-code": "int x;
                     x = 3;",
      "patch-vars": [
        {
          "name": "x",
          "at-entry": {
            "stored-in": "register",
            "register": "R2"
          },
          "at-exit": {
            "stored-in": "memory",
            "frame-pointer": "R11",
            "offset": "0x4:32"
          }
        },
        ...
      ]
    }
  ],
  ...
}
```

In the `patch-vars`, you can also specify a constant value for an identifier
mentioned in the patch code. For instance, suppose your patch code calls
a function `foo`, like this:

```
int x, y;
x = foo(y);
```

If `foo` lives at address `0x100404:32` (i.e., the number `0x100404` with a
bitwidth of `32`), you can specify this in the `patch-vars`:

```
{
  "patches": [
    {
      ...
      "patch-code": "int x;
                     x = foo(y);",
      "patch-vars": [
        {
          "name": "foo",
          "constant": "0x100404:32"
        },
        ...
      ]
    }
  ],
  ...
}
```

If you provide no storage classification for any of the identifiers that
appear in your patch code, VIBES will let the CEGIS loop find appropriate
storage for them.


### Full config file format

Here is the full schema for a config file:

```
{
  "patches" : [
    {
      "patch-name": "<UNIQUE NAME FOR THE PATCH>",
      "patch-point": "<HEX ADDRESS AT WHICH TO START REPLACING BYTES>",
      "patch-size": <NUM BYTES TO REPLACE>,
      "patch-code": "<C-CODE TO COMPILE AND INSERT AT THE PATCH SITE>",
      "patch-sp-align": <(OPTIONAL) NUM BYTES TO ALIGN SP AT START OF PATCH SITE>,
      "patch-vars": [ (OPTIONAL)
        {
          "name": "<NAME OF IDENTIFIER MENTIONED IN patch-code>",
          "at-entry": {
            "stored-in": "register",
            "register": "<REGISTER NAME (IN UPPERCASE)>"
          }
          OR
          {
            "stored-in": "memory",
            "address": "<HEX-NUMBER:BITWIDTH>"
          },
          "at-exit": {
            "stored-in": "register",
            "register": "<REGISTER NAME (IN UPPERCASE)>"
          }
          OR
          {
            "stored-in": "memory",
            "framepointer": "<REGISTER NAME (IN UPPERCASE)>"
            "offset": "<HEX-NUMBER:BITWIDTH>"
          }
        }
        OR
        {
          "name": "<NAME OF IDENTIFIER MENTIONED IN patch-code>",
          "constant": "<HEX-NUMBER:BITWIDTH>""
        },
        ...
      ]
    },
    ...
  ],
  "wp-params": {
    "func": "<FUNCTION NAME TO VERIFY>",
    "postcond": "<SMTLIB2 EXPRESSION ASSERTING A POSTCONDITION OF func>",
    "precond": "<(OPTIONAL) SMTLIB2 EXPRESSION ASSERTING A PRECONDITION OF func>",
    "fun-specs": "<(OPTIONAL) SMTLIB2 FUNCTION SPECS",
    "user-func-specs-orig": "<(OPTIONAL) SMTLIB2 FUNCTION SPECS FOR THE ORIGINAL EXE>",
    "user-func-specs-mod": <(OPTIONAL) SMTLIB2 FUNCTION SPECS FOR THE MODIFIED EXE>",
    "inline": "<(OPTIONAL) BOOL>",
    "ext-solver-path": "<(OPTIONAL) PATH TO ALTERNATE SOLVER>",
    "show": "<(OPTIONAL) SHOW-OPTIONS>",
    "user-fun-input-regs": "<(OPTIONAL) INPUT-REGS>"
  },
  "max-tries": <(OPTIONAL) MAX NUM TIMES TO LET THE CEGIS LOOP RUN>,
  "minizinc-model": "<(OPTIONAL) FILEPATH TO A MINIZINC MODEL>",
  "ogre": "<(OPTIONAL) FILEPATH TO AN OGRE FILE>"
}
```

Here is a description of the above schema:

* `"patches": [PATCH-OBJECTS]` -
  Required. A list of one or more patch objects to apply to the original exe. Each patch object has the following fields:
  * `"patch-name": "NAME"` - Required. A unique name for the patch.
  * `"patch-point": "HEX"` - Required. The address at which to start replacing bytes.
  * `"patch-size": INT` - Required. The number of bytes to replace.
  * `"patch-code": "CODE"` - Required. Code to compile and insert at the patch point. The code should be written in a subset of C.
  * "patch-sp-align": INT` - Number of bytes needed to align the stack pointer at the start of the patch.
  * `"patch-vars": [PATCH-VARS]` - 
    A list of zero or more objects, each of which provides storage classification for identifiers that appear in the provided `patch-code`. Each object specifies a constant, or storage classification for the identifier:
    * For a constant, the object has the following fields:
      * `"name": "NAME"` - Required. The name of the identifier mentioned in the provided `patch-code`.
      * `"constant": "HEX:BITWIDTH"` - Required. A number in hex, with a specified bitwidth (e.g., `0xdeadbeef:32`).
    * For storage classification, the object has the following fields:
      * `"name": "NAME"` - The name of an identifier mentioned in the provided `patch-code`. 
      * `"at-entry": {STORAGE-CLASSIFICATION}` -
        Required. Storage classification for the identifier at the entrance to the patch site. The value can live in a register, or in memory (on the stack).
        * If it's stored in a register, the object has the following fields:
          * `"stored-in": "register"` - Required.
          * "register": "NAME" - Required. The name of a register (in uppercase, e.g., `R0`).
        * If it's stored in memory (on the stack), the object has the following fields:
          * `"stored-in": "memory"` - Required.
          * `"frame-pointer": "NAME"` - Required. The name of the register (in uppercase) that holds the framepointer (e.g., `R11`).
          * `"offset": "HEX:BITWIDTH"` - Required. The offset from the framepointer where the identifier's value lives. This should be number in hex, with a specified bitwidth (e.g., `0xdeadbeef:32`).
      * `"at-exit": {STORAGE-CLASSIFICATION}` -
        Required. Storage classification for the identifier at the exit of the patch site. This should be specified exactly as in `at-entry`.
* `"wp-params": {WP-PARAMS}` -
  Required. A dictionary of parameters to pass to the WP verifier (for more details than what we provide here, see the [documentation](https://draperlaboratory.github.io/cbat_tools/) for [WP](https://github.com/draperlaboratory/cbat_tools/tree/master/wp)). This dictionary can have the following fields:
  * `"func": "NAME"` - Required. The name of the function you want to verify.
  * `"postcond": "SMTLIB2 S-EXP"` - Required. A postcondition that WP should use to verify the correctness of the specified `func` in the patched exe.
  * `"precond": "SMTLIB2 S-EXP"` - A precondition that WP should assume when verifying the correctness of the specified `func` in the patched exe.
  * `"fun-specs": "SMTLIB2 S-EXP"` - Function specs that WP should use in both the original and the modified/patched exes.
  * `"user-func-specs-orig": "SMTLIB2 S-EXP"` - Function specs that WP should use only for the original exe.
  * `"user-func-specs-mod": "SMTLIB2 S-EXP"` - Function specs that WP should use only for the modified/patched exe.
  * `"inline": "BOOL"` - Put `"true"` to tell WP to inline functions, `"false"` otherwise. The default is `"false"`.
  * `"ext-solver-path": "PATH"` - Path to a solver. VIBES will set this to `"boolector"` by default, so if you want WP to use its default (which is Z3), put `"none"` here.
  * `"show": "SHOW-OPTIONS"` - A list of show options (see the WP documentation for details).
  * `"user-fun-input-regs": "INPUT-REGS"` - A list of input registers (see the WP documentation for details).
* `"max-tries": INT` - Number of times to let the CEGIS loop run.
* `"minizinc-model": "FILEPATH"` - The path to a minizinc model filepath. If this is omitted, VIBES will look for the file at `~/.vibes/model.mzn` (which is created for you automatically when you run `make` or `make install`).
* `"ogre": "FILEPATH"` - The path to an ogre file. An ogre file tells VIBES which parts of a binary to lift. If you omit this field altogether, VIBES will simply lift the entire binary. For examples of ogre files, see one of our [examples](resources/exes/arm-stripped-loader/loader.ogre).

Examples of config files can be found in [resources/exes](resources/exes).

