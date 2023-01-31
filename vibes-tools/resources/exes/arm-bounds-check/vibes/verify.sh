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
(let ((base (bvadd init_R0_mod (bvmul init_R1_mod #x00000004))))
  (implies
    (and (= init_mem_orig init_mem_mod)
         (bvugt init_R0_orig init_SP_orig)
         (= init_R0_orig init_R0_mod)
         (= init_R1_orig init_R1_mod))
    (ite (or (bvslt init_R1_mod #x00000000)
             (bvsge init_R1_mod #x00000003))
      (= R0_mod #xffffffff)
      (and (= R0_mod R0_orig)
           (= R0_mod
              (concat
                (select init_mem_mod (bvadd base #x00000003))
                (select init_mem_mod (bvadd base #x00000002))
                (select init_mem_mod (bvadd base #x00000001))
                (select init_mem_mod base))))))))"
}

run
