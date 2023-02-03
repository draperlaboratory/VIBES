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
open Bap_core_theory

(** [to_sexp data ~error] attempts to parse [data] as an S-expression.
    Returns [Error] according to [error] if this fails. *)
val to_sexp :
  string ->
  error:(string -> KB.conflict) ->
  (Sexp.t list, KB.conflict) result
