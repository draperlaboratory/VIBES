(** Replaces higher variables with lower-level locations in patches at
    the BIR level. *)

open !Core_kernel
open Bap_core_theory
open Bap.Std

module KB = Bap_knowledge.Knowledge
module Hvar = Higher_var

exception Subst_err of string

(** [make_reg_name v] will mark the var name [v] as if it were a register. *)
val make_reg_name : string -> string

(** [make_reg v] will mark [v] as a register. *)
val make_reg : var -> var

(** [get_reg_name v] will check if [v] was substituted with a register name,
    and return the original name if this condition is satisfied. Returns
    [None] otherwise. *)
val get_reg_name : string -> string option

(** [undo_reg_name v] will remove the register marker from [v] if it exists. *)
val undo_reg_name : var -> var

(** [substitute tgt h_vars patch_code] replaces higher level variables [h_vars]
    with lower-level locations in the provided [patch_code]. *)
val substitute :
  Theory.target -> Hvar.t list -> blk term list -> blk term list KB.t
