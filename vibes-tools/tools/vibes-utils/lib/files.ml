open Core

module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std

open Vibes_error_lib.Let

let get_lines (filepath : string) : string list =
  let msg = Format.sprintf "Reading lines in '%s'" filepath in
  Log.send msg;
  let lines = In_channel.read_lines filepath in
  let msg = Format.sprintf "Read:\n%s" (String.concat lines ~sep:"\n") in
  Log.send msg;
  lines

let get_lines_or_error (filepath : string) : (string list, Err.t) result =
  try Ok (get_lines filepath)
  with e ->
    let msg = Format.sprintf
      "Could not read file '%s'. %s"
      filepath
      (Exn.to_string e)
    in
    Error (Types.Failed_file_read msg)

(* Get the contents of a file, error if the file is empty. *)
let get_file_contents ~(error : string -> Err.t) (filepath : string)
    : (string, Err.t) result =
  let- lines = get_lines_or_error filepath in
  let contents = String.concat lines ~sep:"\n" in
  if String.is_empty contents then Error (error filepath)
  else Ok contents

(* Get the contents of a file, allow it to be empty. *)
let get_file_contents' (filepath : string) : (string, Err.t) result =
  let- lines = get_lines_or_error filepath in
  Ok (String.concat lines ~sep:"\n")

let write (data : string) (filepath : string) : unit =
  let msg = Format.sprintf "Writing data to '%s'" filepath in
  Log.send msg;
  let () = Out_channel.write_all filepath ~data in
  let msg = Format.sprintf "Wrote:\n%s" data in
  Log.send msg

let write_or_error (data : string) (filepath : string) : (unit, Err.t) result =
  try Ok (write data filepath)
  with e ->
    let msg = Format.sprintf
      "Could not write to file '%s'. %s"
      filepath
      (Exn.to_string e)
    in
    Error (Types.Failed_file_write msg)
