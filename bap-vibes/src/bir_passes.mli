(** 

   Applies a series of transformations to the patch code at the BIR level.
   These transformations include optimization opportunities, massaging the
   shape of the code for the instruction selector, and dealing with the
   limitations of the target architecture.

   Important: Tids of terms that are modified must be preserved. The only
   tids that shall change are the ones that are created fresh.

*)

open Core_kernel
open Bap.Std
open Bap_core_theory

(** The result of running our passes, which is a list of [blk term]s,
    as well as a set of registers to exclude from the Minizinc solution.
    Additionally, [argument_tids] holds the tids of each [def term] where
    an argument to a call was assigned. *)
type t = {
  ir : blk term list;
  exclude_regs : String.Set.t;
  argument_tids : Tid.Set.t;
}

module Opt : sig

  (** [apply ir] applies [Term]-level optimizations to [ir], which are required
      to put the patch code in good shape before instruction selection. *)
  val apply : blk term list -> blk term list

  (** [merge_adjacent ir] attempts to merge adjacent blocks in [ir] with an
      edge in between them. Requires the blocks to be ordered according to a
      reverse post-order DFS traversal. *)
  val merge_adjacent : blk term list -> blk term list
  
end


module Shape : sig

  (** [reorder_blks ir] reorders [ir] according to a reverse post-order DFS
      traversal. *)
  val reorder_blks : blk term list -> blk term list KB.t
  
end

(** [run code ~tgt ~lang ~hvars ~sp_align] creates the BIR from the patch
    code [code], then applies a series of transformations to it. The resulting
    code is then ready to be handed off to the instruction selector. *)
val run :
  insn ->
  tgt:Theory.target ->
  lang:Theory.language ->
  hvars:Higher_var.t list ->
  sp_align:int ->
  t KB.t
  
