(** 

   Applies a series of transformations to the patch code at the BIR level.
   These transformations include optimization opportunities, massaging the
   shape of the code for the instruction selector, and dealing with the
   limitations of the target architecture.   

*)

open Core_kernel
open Bap.Std
open Bap_core_theory

(** The result of running our passes, which is a list of [blk term]s,
    as well as a set of registers to exclude from the Minizinc solution. *)
type t = {
  ir : blk term list;
  exclude_regs : String.Set.t;
}

module Opt : sig

  (** [apply ir] pplies [Term]-level optimizations to [ir], which are required
      to put the patch code in good shape before instruction selection. *)
  val apply : blk term list -> blk term list

end

(** [create code ~tgt ~lang ~hvars ~sp_align] creates the BIR from the patch
    code [code], then applies a series of transformations to it. The resulting
    code is then ready to be handed off to the instruction selector. *)
val create :
  insn ->
  tgt:Theory.target ->
  lang:Theory.language ->
  hvars:Higher_var.t list ->
  sp_align:int ->
  t KB.t
  
