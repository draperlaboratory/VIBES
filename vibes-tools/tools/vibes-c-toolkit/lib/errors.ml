open Bap_core_theory

type KB.Conflict.t +=
  | Other of string

let printer : KB.Conflict.t -> string option = function
  | Other s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
