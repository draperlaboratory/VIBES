open Bap_core_theory

type KB.conflict +=
  | No_patch_code of string
  | Invalid_sexp of string
  | Unknown_target of string

let printer (e : KB.conflict) : string option =
  match e with
  | No_patch_code s -> Some s
  | Invalid_sexp s -> Some s
  | Unknown_target s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
