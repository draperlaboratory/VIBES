(* Implements {!Exe_loader}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge

module KB = Knowledge

let loader = "llvm"

let simple_load (filename : string) =
  let input = Project.Input.file ~loader ~filename in
  Project.create input ~package:filename

let load (filename : string) : Project.t KB.t =
  let input = Project.Input.file ~loader ~filename in
  match Project.create input ~package:filename with
  | Ok proj -> KB.return proj
  | Error e ->
    begin
      let msg = Printf.sprintf "Load error: %s" (Error.to_string_hum e) in
      Errors.fail (Errors.Failed_to_load_proj msg)
    end
