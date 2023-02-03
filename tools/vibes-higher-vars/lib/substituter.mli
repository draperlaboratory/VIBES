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

open Core
open Bap.Std
open Bap_core_theory

module Naming : sig

  (** Same as [mark_reg_name], but doesn't check with the target if the
      name refers to a register. *)
  val mark_reg_name_unsafe : string -> string

  (** Same as [mark_reg], but doesn't check with the target if the variable
      refers to a register. *)
  val mark_reg_unsafe : var -> var

  (** [mark_reg_name target s] marks the string [s] as a register name,
      and returns [Error] if [s] is not a register name according to
      [target]. *)
  val mark_reg_name : Theory.target -> string -> (string, string) result

  (** [mark_reg target v] marks the variable [v] as a register, and
      returns [Error] if [v] is not a register according to [target]. *)
  val mark_reg : Theory.target -> var -> (var, string) result

  (** [unmark_reg_name s] removes the register marking from the
      string [s], if it exists. *)
  val unmark_reg_name : string -> string option

  (** [unmark_reg v] removes the register marking from the variable
      [v], if it exists. *)
  val unmark_reg : var -> var option

  (** [is_reg_name s] returns [true] if string [s] was marked as a
      register name. *)
  val is_reg_name : string -> bool

  (** [is_reg v] returns [true] if variable [v] was marked as a
      register. *)
  val is_reg : var -> bool

end

(** [substitute sub ~hvars ~target] will apply the
    information in [hvars] to [sub]. There are two main phases:

    1. [at_entry] and [at_exit] specifiers for higher variables will be
       translated to initializers and finalizers that are inserted at
       the entry and exit blocks, respectively.

    2. Memory addresses and constants associated are substituted in for
       each use of their associated higher variable
*)
val substitute :
  sub term ->
  hvars:Higher_var.t list ->
  target:Theory.target ->
  sub term KB.t
