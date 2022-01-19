(** Replaces higher variables with lower-level locations in patches at
    the BIR level. *)

open !Core_kernel
open Bap_core_theory
open Bap.Std

module KB = Bap_knowledge.Knowledge
module Hvar = Higher_var

exception Subst_err of string

(** [mark_reg_name v] will mark the var name [v] as if it were a register. *)
val mark_reg_name : string -> string

(** [mark_reg v] will mark [v] as a register. *)
val mark_reg : var -> var

(** [mark_reg_exn tgt v] does the same as [make_reg_name v], but checks to see
    if [v] is actually a register according to [tgt]. Raises [Subst_err] upon
    failure. *)
val mark_reg_exn : Theory.target -> string -> var

(** [unmark_reg_name v] will check if [v] was substituted with a register name,
    and return the original name if this condition is satisfied. Returns
    [None] otherwise. *)
val unmark_reg_name : string -> string option

(** [unmark_reg v] will remove the register marker from [v] if it exists.
    Returns [None] otherwise. *)
val unmark_reg : var -> var option

(** [substitute tgt h_vars patch_code] replaces higher level variables [h_vars]
    with lower-level locations in the provided [patch_code]. *)
val substitute :
  Theory.target -> Hvar.t list -> blk term list -> blk term list KB.t
