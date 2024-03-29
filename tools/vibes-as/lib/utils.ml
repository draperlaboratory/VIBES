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
module Utils = Vibes_utils
module CT = Utils.Core_theory

let asm_printer
    (target : T.target)
    (language : T.language) : (Types.Assembly.printer, KB.conflict) result =
  if CT.is_arm32 target then
    let is_thumb = Utils.Core_theory.is_thumb language in
    Ok (Arm_printer.ir ~is_thumb)
  else if CT.is_ppc32 target then
    Ok Ppc_printer.ir
  else
    let msg = Format.asprintf
        "Unsupported target %a"
        T.Target.pp target in
    Error (Errors.Unsupported_target msg)
