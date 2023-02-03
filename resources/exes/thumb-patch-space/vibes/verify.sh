#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --show=diagnostics \
        --func=main \
        --user-func-specs-orig="g,(assert true),(assert (= R0 (bvadd init_R0 #x0000000a)))" \
        --postcond="(assert (= (bvadd R0_orig #x00000079) R0_mod))"
}

run
