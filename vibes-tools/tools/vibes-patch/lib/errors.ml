open Bap_core_theory

type KB.conflict +=
  | Invalid_asm of string
  | Invalid_binary of string
  | Invalid_patch_point of string
  | Unsupported_target of string
  | No_patch_spaces of string
  | No_patch_room of string

let printer (e : KB.conflict) : string option =
  match e with
  | Invalid_asm s -> Some s
  | Invalid_binary s -> Some s
  | Invalid_patch_point s -> Some s
  | Unsupported_target s -> Some s
  | No_patch_spaces s -> Some s
  | No_patch_room s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
