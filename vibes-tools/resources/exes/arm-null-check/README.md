# Overview

This challenge problem shows how we can insert a null pointer check before we dereference said pointer.

# The vulnerability and the fix

In the original program, the function `patch_fun` allows a dereference of a pointer that could be null.
In the patched program, we want to insert a null check on the pointer.

# Why it matters

We can show that the ternary operator compiles correctly.
