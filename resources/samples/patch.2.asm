((patch_point 66560) (patch_size 2) (directives (".syntax unified"))
 (blocks
  (((label blk0000000b)
    (insns
     ("mov R1, #6" "mov R0, #3"
      "bl (patch_start_label + 4660 - patch_location)")))
   ((label blk0000000a) (insns ("mov R1, R0"))))))
