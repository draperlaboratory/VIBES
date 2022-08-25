open Bap_core_theory

(** Pretty-prints the IR program in ARM assembly format. *)
val ir :
  Vibes_ir.Types.t ->
  is_thumb:bool ->
  (string list, KB.conflict) result
