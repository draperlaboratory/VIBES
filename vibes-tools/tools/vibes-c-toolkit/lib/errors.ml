open Bap_core_theory

type KB.conflict +=
  | Patch_c of string
  | Core_c of string

let printer : KB.conflict -> string option = function
  | Patch_c s -> Some s
  | Core_c s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
