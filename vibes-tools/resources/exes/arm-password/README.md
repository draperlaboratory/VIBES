# Overview

This challenge problem shows how we can remove a branch from the program.

# The vulnerability and the fix

In the original program, the function `patch_fun` will check a password string as its argument against some plaintext, and returns a different value based on whether the password matched or not.
In the patched program, we want to have the function return the same value no matter what.

We change the comparison to `if (0 == 0)`, which gets optimized away by VIBES.

# Why it matters

We can show that the optimizations are working.

# Improvements?

The property we are proving is unsatisfying.
Indeed, it would be better to peak about the behavior of `strcmp` based on the initial value of `R0` when `patch_fun` is called.
