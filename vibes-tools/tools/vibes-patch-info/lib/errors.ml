open Bap_core_theory

type KB.Conflict.t +=
  | Json_parse_error of string

let printer : KB.Conflict.t -> string option = function
  | Json_parse_error s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
