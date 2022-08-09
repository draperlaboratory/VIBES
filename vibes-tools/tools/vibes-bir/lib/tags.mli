open Bap.Std

(** The attribute for [def] terms that are inserted for preserving
    caller-save registers. *)
val spill : unit tag

(** The attribute for [def] terms that set an argument for a function
    call. *)
val argument : unit tag

(** The attribute for [blk] terms that denotes the live-in set. *)
val ins : Var.Set.t tag

(** The attribute for [blk] terms that denotes the live-out set. *)
val outs : Var.Set.t tag

(** The attribute for [sub] terms that denotes the congruence relation
    between variables. *)
val congruences : Var.Set.t Var.Map.t tag

