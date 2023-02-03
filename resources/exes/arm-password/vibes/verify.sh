#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --show=diagnostics \
        --func=patch_fun \
        --precond="(assert (= mem_orig mem_mod))" \
        --postcond="(assert (= R0_mod #x00000000))"
}

run
