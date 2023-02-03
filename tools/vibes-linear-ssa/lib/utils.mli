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

open Bap.Std

type prefix = private string

(** Constructs a prefix based on a tid. *)
val prefix_of_tid : tid -> prefix

(** [linearize ~prefix v] renames [v] with the linear SSA convention,
    according to [prefix].

    For our purposes, [prefix] is the tid string of the block that [v]
    was used in.
*)
val linearize : prefix:prefix -> var -> var

(** [orig_name name] attempts to recover the original variable name of
    of [name], given that it is in linear SSA form. *)
val orig_name : string -> string option

(** [same x y] returns [true] if [x] and [y] are both in linear SSA form
    and they refer to the same original variable name. *)
val same : var -> var -> bool

(** [congruent x y] returns [true] if [x] and [y] are congruent.

    This function is for internal use only, and is only exposed for
    testing.
*)
val congruent : var -> var -> bool
