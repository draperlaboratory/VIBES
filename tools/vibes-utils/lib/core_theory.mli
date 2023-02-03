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

open Bap_core_theory

(** Returns [true] if the language is Thumb. *)
val is_thumb : Theory.language -> bool

(** Returns [true] if the target is 32-bit ARM. *)
val is_arm32 : Theory.target -> bool

(** [get_target name] returns the target with the name [name],
    if it exists. *)
val get_target : string -> (Theory.target, KB.conflict) result

(** [get_language name] returns the language with the name [name],
    if it exists. *)
val get_language : string -> (Theory.language, KB.conflict) result
