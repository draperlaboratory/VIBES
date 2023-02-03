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
open Bap_core_theory

(** [run sub ~target ~language ~patch_info]
    runs the optimization passes on the subroutine [sub]. *)
val run :
  sub term ->
  target:Theory.target ->
  language:Theory.language ->
  patch_info:Vibes_patch_info.Types.t ->
  sub term KB.t
