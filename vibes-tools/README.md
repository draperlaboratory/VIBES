# The VIBES Tool Suite

A suite of tools for the VIBES project.


## The Tools

In progress. So far, the tools that are under development are these:

* `vibes-parse` ([README.md](tools/vibes-parse)) - Takes a `patch.c` file (containing C code), and produces a `patch.bir` file (containing serialized BAP IR).
* `vibes-opt` ([README.md](tools/vibes-opt)) - Takes a `patch.bir` file, and produces a `patch.bir.opt` file (containing optimized BAP IR).


## Building/Installing everything

To build and install the entire suite:

```
make
```

Check that all tools are installed:

```
vibes-parse --help
vibes-opt --help
```

or:

```
vibes-parse --version
vibes-opt --version
```

To just build the suite:

```
make build
```

To install the suite:

```
make install
```

To uninstall and clean:

```
make uninstall
make clean
```


## Building/installing one tool

To build, install, uninstall, or clean a single tool, use `make tool-name`, e.g.:

```
make vibes-parse
```

To build, install, uninstall, or clean the tool, append `.tool-name` to the `build`, `install`, `uninstall`, and `clean` targets. E.g.,

```
make build.vibes-parse
make install.vibes-parse
make uninstall.vibes-parse
make clean.vibes-parse
```


## Toy/sample CLI

There is a toy/dummy command line tool that can be used a a playground or as a template for your own tool.

It lives here:

* [tools/vibes-playground](tools/vibes-playground)

To use it, go to [tools/vibes-playground/bin/main.ml#L21](tools/vibes-playground/bin/main.ml#L21) and start modifying/playing. Then build/install:

```
make vibes-playground
```

And try it out from the command line:

```
vibes-playground --help
vibes-playground
```


## Tool scaffolding

The repo root of this tool suite currently lives at [VIBES-internal/experiments/vibes-tools](https://github.com/draperlaboratory/VIBES-internal/tree/main/experiments/vibes-tools).

The tool suite is then contained in the [tools/](tools/) subdirectory, where each tool has its own folder, like this:

```
|-- <repo root>/
    |
    |-- "README.md" (top level README)
    |-- "Makefile" (top level makefile)
    |-- ...
    |-- "tools/"
        |
        |-- ...
        |-- "vibes-parse/" (folder containing the vibes-parse tool)
        |-- "vibes-opt/" (folder containing the vibes-opt tool)
        |-- "vibes-log/" (folder containing a shared logging library)
        |-- ...
```

Some tools are command line tools. Other tools are just libraries that can be used by other tools. Each tool has a similar scaffolding. It's folder contains:

* A `README.md` describing the tool/library and how to build/install it
* A `Makefile` for building the tool
* A possible `lib` folder, containing local library files just for this tool
* A possible `bin` folder, containing files that define the CLI just for this tool

For instance, consider a command line tool called `vibes-foo`. It would live in its own folder called `vibes-foo/`, which would look like this:

```
|-- <repo root>/
    |
    |-- ...
    |-- "tools/"
        |
        |-- "vibes-foo/"
            |
            |-- "README.md"
            |-- "Makefile"
            |-- "lib/"
            |   |
            |   |-- "dune" (defines "vibes_foo_lib", a non-public library)
            |   |-- "runner.ml" (contains a "run" method, to run the library)
            |   |-- "types.ml" (contains type definitions for this library)
            |   |-- "module_a.ml"
            |   |-- "module_b.ml"
            |   |-- ...
            |-- "bin/"
                |
                |-- "dune" (defines a "main.exe" executable)
                |-- "main.ml" (defines the Cmdliner CLI)
```

Note that a non-public library is defined in the `lib/` folder. Regarding this `lib/` folder, note the following:

* These 'lib/' files make up a library that is not public (no `.opam` file). It is not meant to be installed separately as an opam package. Rather, it is just a library to be used locally by the tool `vibes-foo`.
* Although this local library does not need an `.opam` file, it does need a `dune` file. In that file, we name this local library `vibes_foo_lib`. The convention is to append `_lib` to the end of the tool name (hence, if the tool were called `vibes-bar`, then this library would be named `vibes_bar_lib`).
* If the library can be "run" (as an application), then there should be a `Runner` module that contains a `run` function, so that a CLI frontend can directly "run" it by calling `Vibes_foo_lib.Runner.run`. 
* If the library uses any shared types, they should be declared in a `types.ml` module. 

Note that a `main.exe` executable is defined in the `bin/` folder. Regarding this `bin/` folder, note the following:

* The `bin/` folder is for housing the binary executable `vibes-foo` (a command line tool).
* The CLI should be defined (using `Cmdliner`) in a file called `main.ml`.
* There should be a `dune` file, defining the executable as `main.exe`.
* The `install` target in `<repo root>/tools/vibes-foo/Makefile` should take the built executable `main.exe`, and install it on your system at `${OPAM_SWITCH_PREFIX}/bin`, under the name `vibes-foo`. That way, once the user has installed the tool, they can find the tool by the name `vibes-foo`.

If a tool is just a library meant to be used by other tools (hence it has no CLI frontend of its own), then it should not have a `bin/` folder. Conversely, if a tool is just a CLI frontend that relies entirely on other shared libraries, then it need not have a `lib/` folder.

Other conventions:

* Functionality shared by more than one tool should be moved into its own library.
* Other tools can import shared libraries by listing them in the `libraries` stanza of their local `dune` files. 
* The `<repo root>/tools/vibes-log` library provides a common logger that other tools should use for logging. Messages can be sent from any application by invoking the `Vibes_log.Stream.send` function.
* The `<repo root>/tools/vibes-error` library provides a common error type that other tools should use. The type is extensible, so every tool can add its own custom errors and custom error printers. Such custom errors and printers should be declared in the tools `lib/types.ml` module. In general, any public function from a library should return a `(_, Vibes_error.Std.t) result` type. That way, consumers (i.e., CLI frontends) can automatically handle errors and print them out for the user with an appropriate exit code.
* The `<repo root>/tools/vibes-common-cli-options` library provides various command line options that multiple tools share. For instance, it provides verbosity options that other tools can import into their own CLI.
* The `<repo root>/tools/vibes-constants` library provides constant values that other applications can use. For instance, it specifies the version numbers for the various command line tools. Those CLIs simply import their version number from this `vibes-constants` library.

When in doubt about a convention, look at some of the tools (e.g., `vibes-parse` or `vibes-opt`) and copy what you see there.
