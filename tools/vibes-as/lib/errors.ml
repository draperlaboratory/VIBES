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
  | No_vir of string
  | Invalid_vir of string
  | Unsupported_target of string
  | Printer_error of string

let printer (e : KB.conflict) : string option =
  match e with
  | No_vir s -> Some s
  | Invalid_vir s -> Some s
  | Unsupported_target s -> Some s
  | Printer_error s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
