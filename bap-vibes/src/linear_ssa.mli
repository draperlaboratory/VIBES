(** Converts subroutines into the "linear SSA" form required by the {!Ir}.

    In essence, a subroutine is in linear SSA form when each block of the
    subroutine has all of its variable names unique to it. That is to say,
    no variable names that occur in block [a] also occur in block [b].

    To accomplish this, the [transform] function below simply prefixes
    the variables in each block with the [tid] of their containing block.
    So if [RAX] occurs in a block with [tid = abc], it will become [abc_RAX],
    while if the "same" variable [RAX] occurs in another block [tid = def],
    it will become [def_RAX].
    
    If the "same" variable appears in two different blocks in the manner just
    described, then the {!Ir} considers the two occurrences to be "congruent."
    So two variable names are congruent if they differ only in the containing
    block [tid] prefix. E.g., [abc_RAX] and [def_RAX] are "congruent,"
    because they differ only in the prefixes [abc_] and [def_].

    There is a {!congruent} method below that can be used to easily check
    if two variables are "congruent" in this sense. *)

open Bap.Std

(** [orig_name name] removes the SSA naming conventions from [name].

    Example: if [name] is ["_00001234_R0_1"] then [orig_name name] is
    ["R0"]. Similarly, if [name] is ["_00001234_R0"], then [orig_name name]
    is also ["R0"]. Any other patterns will raise an exception. *)
val orig_name : string -> string

(** [same v1 v2] checks whether [v1] and [v2] have the same [orig_name]. *)
val same : var -> var -> bool

(** [congruent v1 v2] checks whether [v1] and [v2] are "congruent"
    in the sense described in the comment at the beginning of the module.
    [v1] and [v2] must be in the linear SSA form. *)
val congruent : Var.t -> Var.t -> bool

(** [tranform s] takes the subroutine [s] and converts it into the linear
    SSA form described above. *)
val transform : Sub.t -> Sub.t
