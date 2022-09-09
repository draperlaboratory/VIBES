open Bap_core_theory
open Vibes_ir.Types

(** [solve ir target language model_filepath] runs MiniZinc on the [ir]
    program according to the model in [model_filepath], and returns the
    optimized program. *)
val solve :
  t ->
  Theory.target ->
  Theory.language ->
  string ->
  (t, KB.conflict) result
