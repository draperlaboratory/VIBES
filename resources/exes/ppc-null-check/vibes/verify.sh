#!/bin/sh

run () {
    bap wp main main.patched \
        --no-cache \
        --no-objdump \
        --ogre-orig=./vibes/loader.ogre \
        --ogre-mod=./main.patched.ogre \
        --show=diagnostics \
        --func=patch_fun \
        --postcond="(assert
  (implies
    (and (= init_mem_orig init_mem_mod)
         (bvugt init_R3_orig init_R1_orig)
         (= init_R3_orig init_R3_mod))
    (ite (= init_R3_mod #x00000000)
      (= R3_mod #xffffffff)
      (and (= R3_mod R3_orig)
           (= R3_mod
              (concat
                (select init_mem_mod init_R3_mod)
                (select init_mem_mod (bvadd init_R3_mod #x00000001))
                (select init_mem_mod (bvadd init_R3_mod #x00000002))
                (select init_mem_mod (bvadd init_R3_mod #x00000003))))))))"
}

run
