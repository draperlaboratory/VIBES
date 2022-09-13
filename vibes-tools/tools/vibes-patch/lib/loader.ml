open Core
open Bap.Std
open Bap_core_theory

let image
    ?(backend : string = "llvm")
    (filepath : string) : (image, KB.conflict) result =
  match Image.create filepath ~backend with
  | Ok (image, _) -> Ok image
  | Error err ->
    let msg = Format.asprintf "Error loading %s: %a" filepath Error.pp err in
    Error (Errors.Invalid_binary msg)
