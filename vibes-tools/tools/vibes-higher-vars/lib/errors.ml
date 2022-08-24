open Bap_core_theory

type KB.conflict +=
  | Higher_var_not_substituted of string

let printer : KB.conflict -> string option = function
  | Higher_var_not_substituted s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
