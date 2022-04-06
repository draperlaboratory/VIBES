(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

(** Replaces higher variables with lower-level locations in patches at
    the BIR level. *)

open !Core_kernel
open Bap_core_theory
open Bap.Std

module Hvar = Higher_var

exception Subst_err of string

module Naming : sig

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

end

(** [substitute patch_code ~entry_tid ~hvars ~tgt ?spilled] replaces higher
    level variables [hvars] with lower-level locations in the provided
    [patch_code]. *)
val substitute :
  ?spilled:String.Set.t ->
  blk term list ->
  entry_tid:tid ->
  hvars:Higher_var.t list ->
  tgt:Theory.target ->
  blk term list KB.t
