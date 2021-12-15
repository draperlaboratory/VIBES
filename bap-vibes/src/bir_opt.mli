(**

   Applies [Term]-level optimizations required to put the patch code
   in good shape before instruction selection.

*)

open Bap.Std

val apply : blk term list -> blk term list

(** [apply_ordered blks] will attempt to merge adjacent blocks which have an
    edge in between them. For this transformation, [blks] must be ordered
    according to a reverse postorder DFS traversal. *)
val apply_ordered : blk term list -> blk term list
