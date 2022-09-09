open Core
open Bap_core_theory

module T = Theory
module Utils = Vibes_utils

let asm_printer
    (target : T.target)
    (language : T.language) : (Types.Assembly.printer, KB.conflict) result =
  if T.Target.belongs Arm_target.parent target then
    let is_thumb = Utils.Core_theory.is_thumb language in
    Ok (Arm_printer.ir ~is_thumb)
  else
    let msg = Format.asprintf
        "Unsupported target %a"
        T.Target.pp target in
    Error (Errors.Unsupported_target msg)
