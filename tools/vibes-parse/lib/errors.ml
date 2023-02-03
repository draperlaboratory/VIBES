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
  | No_patch_code of string
  | Invalid_sexp of string
  | Unknown_target of string

let printer (e : KB.conflict) : string option =
  match e with
  | No_patch_code s -> Some s
  | Invalid_sexp s -> Some s
  | Unknown_target s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
