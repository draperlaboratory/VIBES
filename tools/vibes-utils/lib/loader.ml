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
open Bap.Std
open Bap_core_theory

let image
    ?(backend : string = "llvm")
    (filepath : string) : (image, KB.conflict) result =
  match Image.create filepath ~backend with
  | Ok (image, _) -> Ok image
  | Error err ->
    let msg = Format.asprintf "Error loading %s: %a" filepath Error.pp err in
    Error (Errors.Invalid_binary msg)
