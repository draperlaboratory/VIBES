open Bap_core_theory

type KB.conflict +=
  | No_blks of string

let printer : KB.conflict -> string option = function
  | No_blks s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
