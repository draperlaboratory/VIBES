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
  | Invalid_asm of string
  | Invalid_address of string
  | Invalid_ogre of string
  | Invalid_insn of string
  | Invalid_size of string
  | Invalid_binary of string
  | Unsupported_target of string
  | No_patch_spaces of string
  | No_disasm of string
  | No_code_segment of string

let printer (e : KB.conflict) : string option =
  match e with
  | Invalid_asm s -> Some s
  | Invalid_address s -> Some s
  | Invalid_ogre s -> Some s
  | Invalid_insn s -> Some s
  | Invalid_size s -> Some s
  | Invalid_binary s -> Some s
  | Unsupported_target s -> Some s
  | No_patch_spaces s -> Some s
  | No_disasm s -> Some s
  | No_code_segment s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
