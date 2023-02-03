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
open Vibes_ir.Types

(** [solve ir target language model_filepath ?constraints] runs MiniZinc
    on the [ir] program according to the model in [model_filepath], and
    returns the optimized program.

    An optional specification of extra [constraints] can be provided as
    a raw string.
*)
val solve :
  ?constraints:string option ->
  t ->
  Theory.target ->
  Theory.language ->
  string ->
  (t, KB.conflict) result
