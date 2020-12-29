(***********************************************************
 *
 * This module contains the Core implementation for the ARM
 * backend. This mostly involves giving a semantics to every
 * Core expression in terms of the [Vibes_ir.t] type.
 *
 * This can be used to generate a [Vibes_ir.t] implementation
 * for anything representable in BAP Core term, e.g. BIL.
 *
 *
 *
 *
 *
 *
 *
 *
 *************************************************************)
open Bap.Std
open Bap_core_theory

(** The ARM implementation of Theory.Core.

   It can be included to write Core terms and directly obtain a
   [Vibes_ir] from it.  *)
module ARM_Core : Theory.Core

(** The abstract representation of [Theory.eff] terms. *)
type arm_eff

(** Deprecated: Work directly with terms parametrized over S : Core *)
module BilARM :
  sig
    val run : ('e, 'r, 's) Theory.Parser.t -> 's list -> unit Theory.eff
  end

(** Deprecated: Work directly with terms parametrized over S : Core *)
val bil_to_arm : (Bil.exp, unit, Bil.stmt) Theory.Parser.t

(** Extracts the ARM semantics from a given KB effect value *)
val effect : 'a Theory.effect -> arm_eff option

(** Extracts the concrete [Vibes_ir] from the abstract [arm_eff]
   representation. *)
val ir : arm_eff -> Vibes_ir.t

(** Pretty prints [ir] terms in a form suitable for assembly *)
val arm_ir_pretty : Vibes_ir.t -> (string list, Errors.t) result