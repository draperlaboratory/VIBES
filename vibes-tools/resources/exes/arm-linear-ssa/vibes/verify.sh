#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --show=diagnostics \
        --func=main \
        --inline=.* \
        --postcond="(assert (= (bvadd R0_orig #x00000006) R0_mod))"
}

run
