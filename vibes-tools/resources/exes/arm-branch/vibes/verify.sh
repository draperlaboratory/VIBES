#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --func=foo \
        --show=diagnostics \
        --postcond="(assert
(implies
  (and (= init_mem_orig init_mem_mod)
       (= init_R0_orig init_R0_mod))
  (ite (= init_R0_mod #x00000000)
       (= R0_mod #x00000001)
       (= R0_mod #x00000002))))"
}

run
