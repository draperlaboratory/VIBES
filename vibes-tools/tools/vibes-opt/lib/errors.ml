open Bap_core_theory

type KB.conflict +=
  | No_bir of string
  | Invalid_bir of string
  | Invalid_func_infos of string
  | No_blks of string
  | No_SP of string
  | Bad_hvar_at_exit of string
  | Stack_loc_already_used of string

let printer (e : KB.conflict) : string option =
  match e with
  | No_bir s -> Some s
  | Invalid_bir s -> Some s
  | Invalid_func_infos s -> Some s
  | No_blks s -> Some s
  | No_SP s -> Some s
  | Bad_hvar_at_exit s -> Some s
  | Stack_loc_already_used s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
