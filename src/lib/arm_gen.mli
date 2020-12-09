open Bap.Std
open Bap_core_theory

module ARM_Core : Theory.Core

type arm_eff

module BilARM :
  sig
    val run : ('e, 'r, 's) Theory.Parser.t -> 's list -> unit Theory.eff
  end

val bil_to_arm : (Bil.exp, unit, Bil.stmt) Theory.Parser.t

val effect : (Theory.Effect.cls, 'a) KB.cls KB.value -> arm_eff option

val ir : arm_eff -> Vibes_ir.t

val arm_ir_pretty : Vibes_ir.t -> string list
