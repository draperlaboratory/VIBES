open Bap_core_theory

type KB.Conflict.t +=
  | Higher_var_not_substituted of string

let printer : KB.Conflict.t -> string option = function
  | Higher_var_not_substituted s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
