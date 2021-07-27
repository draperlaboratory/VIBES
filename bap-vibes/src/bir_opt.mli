(**

   Applies [Term]-level optimizations required to put the patch code
   in good shape before instruction selection.

*)

open Bap.Std

val apply : blk term list -> blk term list
