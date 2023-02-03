#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --func=main \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --show=diagnostics \
        --inline=.* \
        --postcond="(assert (= (bvadd R0_mod #x00000002) R0_orig))"
}

run
