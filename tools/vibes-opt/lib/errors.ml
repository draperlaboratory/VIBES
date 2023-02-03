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
  | No_bir of string
  | Invalid_bir of string
  | Invalid_func_infos of string
  | No_blks of string
  | No_SP of string
  | Bad_hvar_at_exit of string
  | Stack_loc_already_used of string

let printer (e : KB.conflict) : string option =
  match e with
  | No_bir s -> Some s
  | Invalid_bir s -> Some s
  | Invalid_func_infos s -> Some s
  | No_blks s -> Some s
  | No_SP s -> Some s
  | Bad_hvar_at_exit s -> Some s
  | Stack_loc_already_used s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
