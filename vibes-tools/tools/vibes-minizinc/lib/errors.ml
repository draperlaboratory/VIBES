open Bap_core_theory

type KB.conflict +=
  | Deserialization_failed of string
  | Unsupported_role of string
  | Invalid_width of string

let printer (e : KB.conflict) : string option =
  match e with
  | Deserialization_failed s -> Some s
  | Unsupported_role s -> Some s
  | Invalid_width s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
