#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --show=diagnostics \
        --ext-solver-path=boolector \
        --func=main \
        --postcond="(assert (= R0_mod #x00000003))"
}

run
