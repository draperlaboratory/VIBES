open Core_kernel

module Log = Vibes_log_lib.Stream
module Err = Vibes_error_lib.Std

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
