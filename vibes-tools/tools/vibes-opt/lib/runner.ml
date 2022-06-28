open Core_kernel

module T = Bap_core_theory.Theory
module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std
module Utils = Vibes_utils_lib
module Serializers = Vibes_serializers_lib
open Vibes_error_lib.Let

let get_target (name : string) : (T.Target.t, Err.t) result =
  match T.Target.lookup name with
  | None ->
     let msg = Format.sprintf "Unknown target: '%s'" name in
     Error (Types.Unknown_target msg)
  | Some target -> Ok target

let load_from_file (filepath : string) : (string, Err.t) result =
  let- lines = Utils.Files.get_lines_or_error filepath in
  let raw_code = String.concat lines ~sep:"\n" in
  if String.is_empty raw_code then
    let msg = Format.sprintf "No code in file '%s'" filepath in
    Error (Types.No_bir msg)
  else
    Ok raw_code

let to_sexp (data : string) : (Sexp.t list, Err.t) result =
  let lexbuf = Stdlib.Lexing.from_string data in
  try Ok (Sexp.scan_sexps lexbuf)
  with Failure s ->
    let msg = Format.sprintf "Invalid bir S-exp: %s" s in
    Error (Types.Invalid_bir msg)

let run (target : string) (filepath : string) (outfile : string)
    : (unit, Err.t) result =
  let msg = Format.sprintf
    "Vibes_opt_lib.Runner.run '%s' '%s'"
    filepath
    outfile
  in
  Log.send msg;

  let- target = get_target target in
  let- raw_code = load_from_file filepath in
  let- sexps = to_sexp raw_code in

  let- birs =
    Result.all
      (List.map sexps
         ~f:(fun sexp -> Serializers.Bir.deserialize sexp ~target))
  in
  let log_bir bir =
    Log.send (Format.asprintf "BIR:\n%a" Bap.Std.Blk.pp bir)
  in
  List.iter birs ~f:log_bir;

  let sp_align = 0 in
  let hvars = [] in
  let _ : Bir_passes.t = Bir_passes.run birs target sp_align hvars in

  print_endline "TODO: run the optimizer...";

  (* let- () = Utils.Files.write_or_error data outfile in *)

  Ok ()
