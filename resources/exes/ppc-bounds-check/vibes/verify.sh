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
(let ((base (bvadd init_R3_mod (bvmul init_R4_mod #x00000004))))
(implies
  (and (= init_mem_orig init_mem_mod)
       (bvugt init_R3_orig init_R1_orig))
  (ite
    (or (bvslt init_R4_mod #x00000000)
        (bvsge init_R4_mod #x00000003))
    (= R3_mod #xffffffff)
    (and
      (= R3_mod R3_orig)
      (= R3_mod
         (concat
           (select init_mem_mod base)
           (select init_mem_mod (bvadd base #x00000001))
           (select init_mem_mod (bvadd base #x00000002))
           (select init_mem_mod (bvadd base #x00000003)))))))))"
}

run
