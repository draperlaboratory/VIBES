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
  | Unsupported_target of string
  | No_entry_point of string
  | Invalid_target_lang of string

let printer (e : KB.conflict) : string option =
  match e with
  | Unsupported_target s -> Some s
  | No_entry_point s -> Some s
  | Invalid_target_lang s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
