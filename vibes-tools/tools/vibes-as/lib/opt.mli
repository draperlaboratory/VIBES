open Bap.Std
open Vibes_ir.Types

(** Applies generic peephole optimizations to the VIBES IR solution.
    They are parameterized by predicates that are target-specific. *)
val peephole :
  t ->
  is_nop:(Operation.t -> bool) ->
  unconditional_branch_target:(Operation.t -> tid option) ->
  is_move:(Operation.t -> bool) ->
  t
