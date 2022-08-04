open Core
open Bap_core_theory

module Log = Vibes_log_lib.Stream

let (let*) x f = Result.bind x ~f
let (let+) x f = Result.map x ~f

let get_lines (filepath : string) : string list =
  Log.send "Reading lines in '%s'" filepath;
  let lines = In_channel.read_lines filepath in
  Log.send "Read:\n%s" @@ String.concat lines ~sep:"\n";
  lines

let get_lines_or_error
    (filepath : string) : (string list, KB.Conflict.t) result =
  try Ok (get_lines filepath)
  with e ->
    let msg =
      Format.asprintf "Could not read file '%s'. %a" filepath Exn.pp e in
    Error (Errors.Failed_file_read msg)

(* Get the contents of a file, error if the file is empty. *)
let get_file_contents_non_empty
    (filepath : string)
    ~(error : string -> KB.Conflict.t) : (string, KB.Conflict.t) result =
  let* lines = get_lines_or_error filepath in
  if List.is_empty lines then Error (error filepath)
  else Ok (String.concat lines ~sep:"\n")

(* Get the contents of a file, allow it to be empty. *)
let get_file_contents (filepath : string) : (string, KB.Conflict.t) result =
  let+ lines = get_lines_or_error filepath in
  String.concat lines ~sep:"\n"

let write (data : string) (filepath : string) : unit =
  Log.send "Writing data to '%s'" filepath;
  Out_channel.write_all filepath ~data;
  Log.send "Wrote:\n%s" data

let write_or_error
    (data : string)
    (filepath : string) : (unit, KB.Conflict.t) result =
  try Ok (write data filepath)
  with e ->
    let msg =
      Format.asprintf "Could not write to file '%s'. %a" filepath Exn.pp e in
    Error (Errors.Failed_file_write msg)
