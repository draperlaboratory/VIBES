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

type KB.conflict +=
  | Deserialization_failed of string
  | Unsupported_role of string
  | Unsupported_target of string
  | Invalid_width of string

let printer (e : KB.conflict) : string option =
  match e with
  | Deserialization_failed s -> Some s
  | Unsupported_role s -> Some s
  | Unsupported_target s -> Some s
  | Invalid_width s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
