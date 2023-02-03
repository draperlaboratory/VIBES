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
open Vibes_higher_vars

(** [to_core ast target hvars] compiles the parsed C program [ast] into
    a Core Theory program and promises the semantics, which should then
    be available to any of the currently instantiated theories (such as
    BIL, BIR, etc). *)
val to_core :
  Types.ast ->
  Theory.target ->
  Higher_var.t list ->
  Theory.label KB.t
