open Bap.Std

(** The live-in and live-out sets of variables. *)
type ins_outs = {
  ins : Var.Set.t;
  outs : Var.Set.t;
}

(** The mapping from each block to its liveness sets. *)
type ins_outs_map = ins_outs Tid.Map.t

(** The result of the linear SSA transformation.

    - [sub]: the subroutine
    - [ins_outs_map]: the liveness sets for each block.
    - [congruences]: the cngruence relation between
      linear SSA variables.
*)
type t = {
  sub : sub term;
  ins_outs_map : ins_outs_map;
  congruences : var Var.Map.t;
}
