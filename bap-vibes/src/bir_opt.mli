(**

   Applies [Term]-level optimizations required to put the patch code
   in good shape before instruction selection.

*)

open Bap.Std

val apply : blk term list -> blk term list

val apply_ordered : blk term list -> blk term list
