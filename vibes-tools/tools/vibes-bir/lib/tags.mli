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

(** The attribute for [blk] terms that were deliberately created by
    splitting from an existing block. *)
val split : unit tag

(** Returns [true] if this is an attribute that we defined for VIBES. *)
val is_vibes_attr : value -> bool

(** The attribute for [jmp] terms whose destination has an associated
    name. *)
val name_dest : string tag

(** The attribute for [jmp] terms whose destination has an associated
    address. *)
val addr_dest : Bitvec.t tag
