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

open Core
open Bap_core_theory

module T = Theory

let is_thumb (language : T.language) : bool = 
  T.Language.(language = Arm_target.llvm_t32)

let is_arm32 (target : T.target) : bool =
  T.Target.belongs Arm_target.parent target &&
  T.Target.bits target = 32

let is_ppc32 (target : T.target) : bool =
  T.Target.belongs Bap_powerpc_target.parent target &&
  T.Target.bits target = 32

let get_target (name : string) : (T.target, KB.conflict) result =
  match T.Target.lookup name with
  | None ->
    let msg = Format.sprintf "Unknown target: '%s'" name in
    Error (Errors.Unknown_target msg)
  | Some target -> Ok target
 
let get_language (name : string) : (T.language, KB.conflict) result =
  try
    let language = T.Language.read ~package:"bap" name in
    Ok language
  with _ ->
    let msg = Format.sprintf "Unknown language: '%s'" name in
    Error (Errors.Unknown_language msg)
