(***********************************************************
 *
 * This module contains the Core implementation for the ARM
 * backend. This mostly involves giving a semantics to every
 * Core expression in terms of the [Ir.t] type, with
 * instructions specialized to ARM.
 *
 * This can be used to generate a [Ir.t] implementation
 * for anything representable in BAP Core term, e.g. BIL.
 *
 * Effectively, this is what in a traditional compiler is
 * instruction selection.
 *
 *
 *
 *
 *
 *************************************************************)
open Bap_knowledge
open Bap_core_theory

module ARM_Core : Theory.Core
(** The ARM implementation of Theory.Core.

    It can be included to write Core terms and directly obtain a
    [Ir] from it.  *)

type arm_eff
(** The abstract representation of [Theory.eff] terms in [ARM_Core]. *)

val slot : (Theory.Effect.cls, arm_eff option) Knowledge.slot

val effect : 'a Theory.effect -> arm_eff option
(** Extracts the ARM semantics from a given KB effect value *)

val ir : arm_eff -> Ir.t
(** Extracts the concrete [Ir.t] from the abstract [arm_eff]
    representation. *)

val peephole : Ir.t -> Ir.t
(** Performs various ARM specific simplifications of a given [ir]
   program, aimed mostly to conserve space. *)

val gpr : Theory.target -> Theory.language -> Bap.Std.Var.Set.t
(** Returns the set of registers suitable for register allocation on ARM *)

(* FIXME: make this happen at variable creation time *)
val preassign : Theory.language -> Ir.t -> Ir.t
(** Pre-assigns variables according to specific roles (PC, SP, etc) *)

module Pretty : sig
  val arm_ir_pretty : Ir.t -> (string list, Kb_error.t) result
  (** Pretty prints [Ir.t] terms in a form suitable for assembly *)
end
