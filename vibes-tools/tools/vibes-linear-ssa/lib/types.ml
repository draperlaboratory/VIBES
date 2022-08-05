open Bap.Std

type ins_outs = {
  ins : Var.Set.t;
  outs : Var.Set.t;
}

type ins_outs_map = ins_outs Tid.Map.t

type t = {
  sub : sub term;
  ins_outs_map : ins_outs_map;
  congruences : var Var.Map.t;
}
