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

module Pre = Types.Preassign

let preassign ~(is_thumb : bool) : Pre.transform =
  fun typ -> function
    | "FP" when is_thumb -> Arm_env.r7
    | "FP" -> Arm_env.r11
    | n -> Var.create n typ
