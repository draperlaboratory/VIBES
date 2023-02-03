# vibes-patch

This command line tool takes serialized assembly, an existing binary, and a patch information structure as input.
Based on the patch information, it will use the system assembler to generate object code for the patch, and attempt to find a suitable patch space in the binary for it.
The output is the patched binary, if successful.
