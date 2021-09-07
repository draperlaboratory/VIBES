(***********************************************************
 *
 * This module contains the instruction selection for the ARM
 * backend. This mostly involves using the "maximal munch"
 * approach from the Tiger Book, pattern matching on expressions
 * and statements. We generate low level instructions of the
 * [Ir.t] type, with opcodes specialized to ARM.
 *
 * This can be used to generate a [Ir.t] implementation
 * for anything that has a BAP [Semantics.t] slot, e.g. BIL.
 *
 *
 *
 *************************************************************)
open Bap_core_theory
open Bap.Std

(** The abstract representation of [Theory.eff] terms in [ARM_Core]. *)
type arm_eff

(** Extracts the concrete [Ir.t] from the abstract [arm_eff]
    representation. *)
val ir : arm_eff -> Ir.t

(** Performs various ARM specific simplifications of a given [ir]
   program, aimed mostly to conserve space. *)
val peephole : Ir.t -> Ir.t

(** Returns the set of *all* registers on ARM *)
val regs : Theory.target -> Theory.language -> Bap.Std.Var.Set.t

(** Returns the set of registers suitable for register allocation on ARM *)
val gpr : Theory.target -> Theory.language -> Bap.Std.Var.Set.t

(** Pre-assigns variables according to specific roles (PC, SP, etc) *)
(* FIXME: make this happen at variable creation time *)
val preassign : Theory.target -> Theory.language -> Ir.t -> Ir.t


module Pretty :
sig
  (** Pretty prints [Ir.t] terms in a form suitable for assembly *)
  val arm_ir_pretty : Ir.t -> (string list, Kb_error.t) result
end

module ARM_Gen :
sig
  val select : blk term list -> Ir.t
end
