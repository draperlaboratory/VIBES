open Bap.Std
open Bap_core_theory

(** Runs the ARM instruction selector on the BIR program. *)
val select : sub term -> is_thumb:bool -> Vibes_ir.Types.t KB.t
