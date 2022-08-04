open Bap_core_theory

type KB.Conflict.t +=
  | No_blks of string

let printer : KB.Conflict.t -> string option = function
  | No_blks s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
