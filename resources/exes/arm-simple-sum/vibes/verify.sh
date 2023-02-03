#!/bin/sh

run () {
    bap wp main main.patched \
        --no-objdump \
        --no-cache \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --show=diagnostics \
        --func=main \
        --postcond="(assert (= (bvadd R0_orig #x00000004) R0_mod))"
}

run
