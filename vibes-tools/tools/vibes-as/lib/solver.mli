open Bap_core_theory
open Vibes_ir.Types

(** [solve ir target language model_filepath ?constraints] runs MiniZinc
    on the [ir] program according to the model in [model_filepath], and
    returns the optimized program.

    An optional specification of extra [constraints] can be provided as
    a raw string.
*)
val solve :
  ?constraints:string option ->
  t ->
  Theory.target ->
  Theory.language ->
  string ->
  (t, KB.conflict) result
