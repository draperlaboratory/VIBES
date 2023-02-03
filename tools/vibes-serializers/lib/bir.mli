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

(** Serializes a var. *)
val serialize_var : var -> Sexp.t

(** Serializes a subroutine. *)
val serialize : sub term -> Sexp.t

(** Deserializes a subroutine. *)
val deserialize : Sexp.t -> sub term KB.t
