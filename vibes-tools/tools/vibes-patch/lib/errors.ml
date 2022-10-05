open Bap_core_theory

type KB.conflict +=
  | Invalid_asm of string
  | Invalid_binary of string
  | Invalid_address of string
  | Invalid_ogre of string
  | Invalid_insn of string
  | Invalid_size of string
  | Unsupported_target of string
  | No_patch_spaces of string
  | No_disasm of string

let printer (e : KB.conflict) : string option =
  match e with
  | Invalid_asm s -> Some s
  | Invalid_binary s -> Some s
  | Invalid_address s -> Some s
  | Invalid_ogre s -> Some s
  | Invalid_insn s -> Some s
  | Invalid_size s -> Some s
  | Unsupported_target s -> Some s
  | No_patch_spaces s -> Some s
  | No_disasm s -> Some s
  | _ -> None

let () = KB.Conflict.register_printer printer
