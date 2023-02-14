#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --show=diagnostics \
        --inline=.* \
        --func=main \
        --postcond="(assert (= R3_orig (bvadd R3_mod #x00000002)))"
}

run
