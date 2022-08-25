open Bap_core_theory

type KB.conflict +=
  | No_bir of string
  | Invalid_bir of string
  | Unsupported_target of string
  | Selector_error of string

let printer (e : KB.conflict) : string option =
  match e with
  | No_bir s -> Some s
  | Invalid_bir s -> Some s
  | Unsupported_target s -> Some s
  | Selector_error s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
