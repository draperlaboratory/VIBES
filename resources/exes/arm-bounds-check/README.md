# Overview

This challenge problem shows how we can insert a bounds check on an array lookup.

# The vulnerability and the fix

In the original program, the function `patch_fun` allows an out-of-bounds access.
In the patched program, we want to insert a bounds check on the array.

# Why it matters

We can show off our use of somewhat complex conditional statements (including a short-circuiting AND).

# Caveats

Since the patch has no hope of fitting inside the space of the original function, we insert the dummy region into the binary at link-time.
However, we need to strip the symbol tables, since BAP will lift the dummy region as a separate function and this will make our lives difficult for verifying the patch.

# Improvements?

The generated code for the short-circuiting AND is naiive and could be made more efficient (especially wrt. the size of the generated assembly code).
Probably, this would be an optimization pass at the BIR level, rather than having to change the C frontend.
