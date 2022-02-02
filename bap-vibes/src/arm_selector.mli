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
open Core_kernel
open Bap_core_theory
open Bap.Std

module Congruent_temps : sig

  type cls
  type t = cls KB.obj
  type computed = (cls, unit) KB.cls KB.value

  val name : string
  val cls : (cls, unit) KB.cls
  val slot : (cls, (Ir.op_var * Ir.op_var) option) KB.slot
  
end

(** The abstract representation of [Theory.eff] terms in [ARM_Core]. *)
type arm_eff

(** Returns [true] if the encoding is ARM.
    Returns a KB error if the encoding is unknown. *)
val is_arm : Theory.language -> bool KB.t

(** Returns [true] if the encoding is Thumb.
    Returns a KB error if the encoding is unknown. *)
val is_thumb : Theory.language -> bool KB.t

(** Returns [true] if the encoding is ARM or Thumb.
    Returns a KB error if the encoding is unknown. *)
val is_arm_or_thumb : Theory.language -> bool KB.t

(** Extracts the concrete [Ir.t] from the abstract [arm_eff]
    representation. *)
val ir : arm_eff -> Ir.t KB.t

(** Performs various ARM specific simplifications of a given [ir]
   program, aimed mostly to conserve space. *)
val peephole : Ir.t -> Ir.t

(** Returns the set of *all* registers on ARM *)
val regs : Theory.target -> Theory.language -> Bap.Std.Var.Set.t

(** Returns the set of registers suitable for register allocation on ARM *)
val gpr : Theory.target -> Theory.language -> Bap.Std.Var.Set.t KB.t

(** Pre-assigns variables according to specific roles (PC, SP, etc) *)
(* FIXME: make this happen at variable creation time *)
val preassign : Theory.target -> Ir.t -> is_thumb:bool -> Ir.t

module Isel : sig
  val patterns : Isel.info
end

module Pretty :
sig
  (** Pretty prints [Ir.t] terms in a form suitable for assembly *)
  val arm_ir_pretty : Ir.t -> (string list, Kb_error.t) result
end

module ARM_Gen :
sig
  (** [select blks ~is_thumb ~argument_tids] performs instruction selection:
      it translates the sequence of BIR blocks [blks] to the appropriate ARM
      opcodes (keeping the operands abstract). [is_thumb] indicates whether
      the Thumb encoding is being targeted. [argument_tids] indicates the
      tids where function arguments are set before a call. *)
  val select :
    blk term list ->
    is_thumb:bool ->
    argument_tids:Tid.Set.t ->
    Ir.t KB.t
end
