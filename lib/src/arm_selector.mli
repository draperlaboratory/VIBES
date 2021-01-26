(***********************************************************
 *
 * This module contains the Core implementation for the ARM
 * backend. This mostly involves giving a semantics to every
 * Core expression in terms of the [Vibes_ir.t] type, with
 * instructions specialized to ARM.
 *
 * This can be used to generate a [Vibes_ir.t] implementation
 * for anything representable in BAP Core term, e.g. BIL.
 *
 * Effectively, this is what in a traditional compiler, is
 * instruction selection.
 *
 *
 *
 *
 *
 *************************************************************)
open Bap_knowledge
open Bap_core_theory

(** The ARM implementation of Theory.Core.

    It can be included to write Core terms and directly obtain a
    [Vibes_ir] from it.  *)
module ARM_Core : Theory.Core

(** The abstract representation of [Theory.eff] terms. *)
type arm_eff

val slot : (Theory.Effect.cls, arm_eff option) Knowledge.slot

(** Extracts the ARM semantics from a given KB effect value *)
val effect : 'a Theory.effect -> arm_eff option

(** Extracts the concrete [Vibes_ir] from the abstract [arm_eff]
    representation. *)
val ir : arm_eff -> Ir.t

module Pretty :
sig
  (** Pretty prints [ir] terms in a form suitable for assembly *)
  val arm_ir_pretty : Ir.t -> (string list, Errors.t) result
end
