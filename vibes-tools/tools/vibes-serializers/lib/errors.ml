open Bap_core_theory

type KB.conflict +=
  | Unhandled_bir of string
  | Invalid_bir of string
  | Invalid_func_info of string

let printer : KB.conflict -> string option = function
  | Unhandled_bir s -> Some s
  | Invalid_bir s -> Some s
  | Invalid_func_info s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
