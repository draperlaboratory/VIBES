(* Implements {!Utils}. *)
open Bap_knowledge
module KB = Knowledge

let cp (src_filepath : string) (dst_filepath : string) : unit =
  let buffer_size = 1026 in
  let buffer = Bytes.create buffer_size in
  let stats = Unix.stat src_filepath in
  let read_restrictions = [Unix.O_RDONLY] in
  let write_restrictions = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  let fd_in = Unix.openfile src_filepath read_restrictions 0 in
  let fd_out = Unix.openfile dst_filepath write_restrictions stats.st_perm in
  let rec copy_loop () = match Unix.read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r -> let _ : int = (Unix.write fd_out buffer 0 r) in copy_loop ()
  in
  copy_loop ();
  Unix.close fd_in;
  Unix.close fd_out

let run_process_exn (command : string) (args : string list) : unit KB.t =
  let (as_stdout, as_stdin) =
    Unix.open_process (String.concat " " (command :: args)) in
  let status = Unix.close_process (as_stdout, as_stdin) in
  match status with
  | WEXITED 0 -> KB.return ()
  | WEXITED 127 ->
    begin
      let msg = Format.sprintf "'%s' not found in PATH" command in
      Errors.fail (Errors.Command_not_found msg)
    end
  | WEXITED n ->
    begin
      let msg = Format.sprintf "%s returned exit code: %d" command n in
      Errors.fail (Errors.Exit_code msg)
    end
  | _ ->
    begin
      let msg =
        Format.sprintf "%s exited with unknown return status" command in
      Errors.fail (Errors.Unexpected_exit msg)
    end
