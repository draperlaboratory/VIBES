open Bap.Std
open Bap_core_theory
open Vibes_ir.Types

(** The destinations for a control-flow operation in VIBES IR. *)
type dsts = {
  dst : Operand.t;
  ret : Operand.t option;
}

(** Translate a tid into the appropriate VIBES IR operand. *)
val get_label : tid -> Operand.t KB.t

(** Extract the VIBES IR operands from a BIR jump.  *)
val get_dsts : jmp term -> dsts option KB.t
