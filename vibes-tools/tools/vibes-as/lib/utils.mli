open Bap_core_theory

(** Returns the assembly printer based on the target. *)
val asm_printer :
  Theory.target ->
  Theory.language ->
  (Types.Assembly.printer, KB.conflict) result
