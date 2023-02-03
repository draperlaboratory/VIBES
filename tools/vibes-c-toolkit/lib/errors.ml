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
  | Parse_c of string
  | Patch_c of string
  | Core_c of string

let printer : KB.conflict -> string option = function
  | Parse_c s -> Some s
  | Patch_c s -> Some s
  | Core_c s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
