#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --ext-solver-path=boolector \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --show=diagnostics \
        --func=patch_fun \
        --postcond="(assert
  (implies
    (and (= init_mem_orig init_mem_mod)
         (bvugt init_R0_orig init_SP_orig)
         (= init_R0_orig init_R0_mod))
    (ite (= init_R0_mod #x00000000)
      (= R0_mod #xffffffff)
      (and (= R0_mod R0_orig)
           (= R0_mod
              (concat
                (select init_mem_mod (bvadd init_R0_mod #x00000003))
                (select init_mem_mod (bvadd init_R0_mod #x00000002))
                (select init_mem_mod (bvadd init_R0_mod #x00000001))
                (select init_mem_mod init_R0_mod)))))))"
}

run
