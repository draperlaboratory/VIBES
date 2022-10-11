# VIBES

VIBES (Verified, Incremental Binary Editing with Synthesis) is a suite of command line tools that uses program synthesis and constraint programming techniques to compile a source-level patch and insert it into a preexisting binary program. VIBES uses formal verification to prove that only the intended change is made.

The vibes tool chain consists of the following command line tools:

* `vibes-init` - scaffolds a VIBES patching project.
* `vibes-parse` - parses a patch written in C (or rather, a subset of C).
* `vibes-opt` - runs optimization passes over a parsed patch.
* `vibes-select` - selects low-level instructions for a parsed patch.
* `vibes-as` - assembles the low-level instructions into a binary program fragment.
* `vibes-patch` - surgically inserts the binary program fragment into the original binary.

To patch a binary program, use `vibes-init` to scaffold a patching project, edit a few generated files to tell VIBES how to patch your program, and then run `make`. The Makefile will run the above tools in sequence, passing the output of each as input to the next.

Each of the above tools takes its input as human-readable and editable files, so you can run any of these tools on their own, if you want to tinker. Each tool has a `--help` command that provides details about usage.

For a brief overview and example of how to use VIBES, see the tutorial:

* [Tutorial](docs/tutorial/Tutorial.md)

VIBES is open source. If you want to contribute, see the following:

* [Contributing](docs/Contributing.md)


## Acknowledgements

This work is sponsored by DARPA / NAVWAR Contract N6600120C4018, as part of the DARPA Assured Micro-Patching (AMP) program. Its content does not necessarily reflect the position or policy of the US Government and no official endorsement should be inferred.


## Docker Installation

To run VIBES with Docker, navigate into this directory, and build the image:

```
docker build --tag vibes:latest .
```

By default the build will utilize `nproc` jobs. If you want to limit that, specify `OPAM_JOBS` as a build argument:

```
docker build --build-arg OPAM_JOBS=3 --tag vibes:latest .
```

If you need to build behind a proxy, provide `http_proxy` and `https_proxy` variables as build arguments too:

```
docker build --build-arg http_proxy=http://myproxy.com:1234 --build-arg https_proxy=http://myproxy.com:1234 --tag vibes:latest .
```

To get a bash prompt inside the container:

```
docker run -ti vibes:latest bash
```

Once inside, you can run any of the VIBES tools, e.g.:

```
vibes-init --help
```

Normally, you want VIBES to operate on files on your local machine. To mount a local directory (e.g., your home directory) to `/external` inside the container, use Docker`s `-v` flag:

```
docker run -ti -v ${HOME}:/external -w /external vibes:latest bash
```

The container user is `opam`, with UID 1000. If you are not locally running as the user with UID 1000, then add `${HOME}` to the 1000 group, so that the container user will be able to write to it:

```
sudo chown :1000 ${HOME}
```


## Manual Installation

For manual installation, the following instructions are for Ubuntu 20.04. First, install OCaml and create a 4.14 switch. Then navigate into this directory, run the setup script and source the `update-PATH` script:

```
bash bin/setup/ubuntu.bash
. bin/setup/update-PATH.bash
``` 

This will install all required APT packages and all required OPAM packages, it will install BAP and CBAT, it will install minizinc and boolector, and it will update the PATH so your system can find minizinc and boolector.


## Usage

In a folder somewhere (accessible to the Docker user if you are running VIBES in a container), create a folder called `workspace`:

```
mkdir workspace
cd workspace
```

Move the binary executable you want to patch into that directory:

```
mv /path/to/program.exe .
```

Run `vibes-init`:

```
vibes-init \
  --patch-name=my-patch \
  --binary=program.exe \
  --patched-binary=program.patched.exe \
  --target=bap:armv7+le \
  --language=bap:llvm-armv7
```

That will generate a number of files.

Open `my-patch.info.json` and adjust the patch-point and patch-size, e.g.:

```
{
  "patch-point": "0x103c8:32u",
  "patch-size": 4,
  "overwrite": true,
  "sp-align": 0,
  "patch-vars": []
}
```

That tells VIBES to start patching at the address `0x103c8:32u` (the `32u` says this is a 32-bit unsigned number).

Open `my-patch.c` and add whatever patch code you want VIBES to insert into the original program, e.g.:

```
int x = 3;
```

Now run `make`:

```
make
```

VIBES will compile your code in `my-patch.c` and insert it into the binary `program.exe` at the address `0x103c8`. It will save the new binary as `program.patched.exe`.
