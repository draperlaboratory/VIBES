open Bap_core_theory

type KB.conflict +=
  | No_vir of string
  | Invalid_vir of string
  | Unsupported_target of string
  | Printer_error of string

let printer (e : KB.conflict) : string option =
  match e with
  | No_vir s -> Some s
  | Invalid_vir s -> Some s
  | Unsupported_target s -> Some s
  | Printer_error s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
