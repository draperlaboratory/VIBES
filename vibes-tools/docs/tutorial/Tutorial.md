# Tutorial

To get an idea of how to use VIBES, run through this tutorial.


## Installation

Follow the instructions in [README.md](../../README.md) to install the VIBES tool suite.

In this tutorial, we'll work inside your local home directory, so if you are using VIBES inside a Docker container, make sure that your home directory belongs to the 1000 group, and mount your home directory inside the container:

```
sudo chown :1000 ${HOME}
docker run --rm -ti -v ${HOME}:/external -w /external vibes:latest bash
```


## Set Up The Project

Create a folder in your home directory called `workspace`:

```
mkdir ${HOME}/workspace
cd ${HOME}/workspace
```

Now we have a place we can work.

Next, we need a binary executable that we can patch. To keep things simple, let's create a simple 32-bit ARM executable that returns an exit code of 5.

Create a file called `main.c` with the following contents:

```
int main() {
  int x = 5;
  return x;
}
```

Build it:

```
arm-linux-gnueabi-gcc -O1 -marm -o main.exe main.c
```

Run it and check the exit code:

```
QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main.exe
echo $? # should return 5
```


### Put Together a Patch

Let's patch the above program and make it return `3` instead of `5`.

A patch consists of two files: 

1. A patch-info file: this contains information about the patch site (e.g., the address to start patching at, how many bytes to overwrite, etc.)
2. A code file: this contains the new code you want to insert into the original program

First, we need to figure out which instructions in the binary program we want to overwrite. Use objdump to look at the disassembly:

```
arm-linux-gnueabi-objdump -Ds main.exe | less
```

Find the `<main>` function. It should look something like this:

```
000103c8 <main>:
   103c8:       e3a00005        mov     r0, #5
   103cc:       e12fff1e        bx      lr
```

Notice the instruction at `0x103c8`: the number `#5` is placed in the register `R0` (the return register). This implements the code `int x = 5`: in this case, the value of the variable `x` is stored in the register `R0`.

Since we want to make this program return `3` instead of `5`, the instruction at `0x103c8` is a reasonable instruction to overwrite. So, make a note of the following:

* The address we want to start patching at is `0x103c8` (this is the patch-point)
* We can overwrite the entire instruction, i.e., 4 bytes (this is the patch-size).

Next, we will need new code to insert into the binary. In this case, we can use this:

```
int x = 3;
```

Here we simply set `x` to `3`. The program will then return that value.

Finally, make a note of the target architecture and the encoding language of the binary program at the patch site. For 32-bit ARM, we can use these:

* Encoding language: `bap:llvm-armv7`
* Target: `bap:armv7+le` (to see all available targets, run `bap list targets` from the command line)


### Patch the Program, The Easy Way

The `vibes-init` tool will scaffold a patching project. 

To begin, run `vibes-init`:

```
vibes-init \
  --patch-names=my-patch \
  --binary=main.exe \
  --patched-binary=main.patched.exe \
  --target=bap:armv7+le \
  --language=bap:llvm-armv7 \
  --verbose
```

This will generate a number of files. At a minimum, you need to edit `*.info.json` and `*.c` files.
 
First, open `my-patch.info.json` and change it to this:

```
{
  "patch-point": "0x103c8:32",
  "patch-size": 4,
  "overwrite": true,
  "sp-align": 0,
  "patch-vars": [
    {
      "name": "x",
      "storage-class": ["register", {"at-entry": "R0", "at-exit": "R0"}]
    }
  ]
}
```

This tells VIBES to start patching at `0x103c8` (a 32-bit address) and to overwrite 4 bytes. It also tells VIBES that there is a variable that we care about, called `x`, which lives in register `R0` at the entry and exit of the patch site. 

Next, open `my-patch.c` and add the patch code:

```
int x = 3;
```

Now that we have filled in the patch-info file and provided some code for the patch, we are ready to patch the program. Run `make`:

```
make
```

If all goes well, VIBES will produce a patched executable called `main.patched.exe`. You can run it and check the exit code to confirm that it returns 3 (instead of 5):

```
chmod +x main.patched.exe
QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main.patched.exe
echo $? # It should return 3
```

To confirm that VIBES correctly patched the program, use `objdump` to look at the assembly of the patched binary:

```
arm-linux-gnueabi-objdump -Ds main.patched.exe | less
```

Find the function `<main>`. You should see something like this:

```
000103c8 <main>: 
   103c8:       e3a00003        mov     r0, #3
   103cc:       e12fff1e        bx      lr 
```

You can see that, at address `0x103c8`, the instruction now stores the value `#3` (rather than `#5`) in the register `R0`, as expected.


### Patch the Program, Step by Step

Using `vibes-init` is convenient, but you can also run each step of the VIBES tool chain manually. This section goes through each step individually.


### Parse the Patch

First, create a file called `patch.info.json` with these contents:

```
{
  "patch-point": "0x103c8:32",
  "patch-size": 4,
  "sp-align": 0,
  "patch-vars": [
    {
      "name": "x",
      "storage-class": ["register", {"at-entry": "R0", "at-exit": "R0"}]
    }
  ]
}
```

This tells VIBES the following things:

* `patch-point` start patching at address `0x103d4:32` (the `32` indicates that the address is a 32-bit number)
* `patch-size`: overwrite 4 bytes
* `sp-align`: no need to make any adjustments at the patch point to align the stack pointer
* `patch-vars`: this tells VIBES where variables live. In this case, this tells VIBES that there is only one variable that we care about, called `x`, and it lives in the register `R0` at the entrance and exit of the patch site.

Next, create a file called `patch.c` with these contents:

```
int x = 3;
```

Now, parse the patch:

```
vibes-parse \
  --patch-info-filepath=patch.info.json \
  --patch-filepath=patch.c \
  --bir-outfile=patch.bir \
  --function-info-outfile=patch.func.json \
  --target=bap:armv7+le \
  --verbose
```

This tells `vibes-parse` to parse the patch file `patch.c`. We provide the patch-info file `patch.info.json`, and we tell it the target architecture.

For output, `vibes-parse` will write a BIR file called `patch.bir` (BIR is BAP's Intermediate Representation language), and it will also dump certain useful information about functions in a file called `patch.func.json`.  

The BIR file and the function-info file are both human-readable. You can inspect them if you like.


### Optimize the Patch

In the last step, we used `vibes-parse` to parse the patch code into BIR. In this step, we will run some optimization passes over that BIR.

Run the optimization pass:

```
vibes-opt \
  --patch-info-filepath=patch.info.json \
  --function-info-filepath=patch.func.json \
  --bir-filepath=patch.bir \
  --bir-outfile=patch.opt.bir \
  --target=bap:armv7+le \
  --language=bap:llvm-armv7 \
  --verbose
```

This tells `vibes-opt` to optimize the BIR in `patch.bir`. We also give it the function-info file `patch.func.json` and the patch-info file `patch.func.json`, and we tell it the target and the language encoding.

For output, `vibes-opt` will write a new BIR file called `patch.opt.bir`, which contains the optimized version of the BIR patch. This too is a human-readable file, so you can inspect it if you like.


### Select Low-level Instructions

In the next step, we ask VIBES to convert the optimized BIR into VIBES IR. In effect, this is the VIBES instruction selector. 

Run the instruction selector:

```
vibes-select
  --patch-info-filepath=patch.info.json \
  --bir-filepath=patch.opt.bir \
  --vir-outfile=patch.vir \
  --target=bap:armv7+le \
  --language=bap:llvm-armv7 \
  --verbose
```

This tells `vibes-select` to take the BIR in `patch.opt.bir` and convert it to VIBES IR. We also give it the patch-info file `patch.info.json`, and we tell it the target and language.

For output, `vibes-select` will write the generated VIBES IR to a file called `patch.vir`. The `patch.vir` file is human-readable, so you can inspect it if you like.


### Assemble the Low-Level Instructions

In the next step, we ask VIBES to take the VIBES IR we produced in the last step and turn it into assembly.

Run the assembler:

```
vibes-as \
  --patch-info-filepath=patch.info.json
  --vir-filepath=patch.vir \
  --asm-outfile=patch.asm \
  --target=bap:armv7+le \
  --language=bap:llvm-armv7 \
  --verbose
```

This tells `vibes-as` to take the VIBES IR in `patch.vir` and convert it into assembly. We also give it the patch-info file `patch.info.json`, and we tell it the target and the language.

For output, `vibes-as` writes the generated assembly to a file called `patch.asm`.


### Insert the patch

In the next step, we ask VIBES to insert the assembly into the original program.

Run the patcher:

```
vibes-patch \
  --asm-filepaths=patch.asm \
  --binary=main.exe \
  --patched-binary=main.patched.exe \
  --target=bap:armv7+le \
  --language=bap:llvm-armv7 \
  --verbose 
```

This tells `vibes-patch` to take the assembly in `patch.asm` and insert it into the original binary at `main.exe`.

For output, `vibes-patch` will write the new, patched binary to `main.patched.exe`.

If all went well, we should be able to run this binary, and see that it now returns an exit code of `3` rather than `5`:

```
chmod +x main.patched.exe
QEMU_LD_PREFIX=/usr/arm-linux-gnueabi qemu-arm main.patched.exe
echo $? # It should return 3
```

To confirm, use `objdump` to look at the assembly of the patched binary:

```
arm-linux-gnueabi-objdump -Ds main.patched.exe | less
```

Find the function `<main>`. You should see something like this:

```
000103c8 <main>: 
   103c8:       e3a00003        mov     r0, #3
   103cc:       e12fff1e        bx      lr 
```

You can see that, at address `0x103c8`, the instruction now stores the value `#3` (rather than `#5`) in the register `R0`, as expected.
