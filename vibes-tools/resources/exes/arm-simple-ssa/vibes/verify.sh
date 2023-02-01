#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --func=main \
        --show=diagnostics \
        --postcond="(assert (= (bvadd R0_orig #x00000002) R0_mod))"
}

run
