# vibes-parse

This command line tool takes a patch (written in a subset of C), and it parses it into BIR (BAP's Intermediate Language).

For input, it takes a file (containing the patch code), and as output, it writes a file (containing the serialized BIR), as well as a file containing information about function args in the original patch code.


## Building and installing

To build:

```
make build
```

To install:

```
make install
```

To uninstall:

```
make uninstall
```

To clean:

```
make clean
```
