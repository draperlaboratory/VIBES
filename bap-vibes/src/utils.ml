(* Implements {!Utils}. *)

open Bap.Std
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

(* [lift_kb] lifts the Result monad to the KB monad *)
let lift_kb_result (x : ('a, Errors.t) Result.t) : 'a KB.t =
  match x with
  | Ok x -> KB.return x
  | Error e -> Errors.fail e

let run_process (command : string) (args : string list)
    : (unit, Errors.t) Result.t =
  let (as_stdout, as_stdin) =
  Unix.open_process (String.concat " "  (command :: args)) in
  let status = Unix.close_process (as_stdout, as_stdin) in
  match status with
  | WEXITED 0 -> Ok ()
  | WEXITED 127 ->
    begin
      let msg = Format.sprintf "'%s' not found in PATH" command in
      Error (Errors.Command_not_found msg)
    end
  | WEXITED n ->
    begin
      let msg = Format.sprintf "%s returned exit code: %d" command n in
      Error (Errors.Exit_code msg)
    end
  | _ ->
    begin
      let msg =
        Format.sprintf "%s exited with unknown return status" command in
      Error (Errors.Unexpected_exit msg)
    end

let load_exe (filename : string)
    : (project * Program.t, Toplevel_error.t) result =
  let input = Project.Input.file ~loader:"llvm" ~filename in
  match Project.create input ~package:filename with
  | Ok proj ->
    begin
      let prog = Project.program proj in
      Ok (proj, prog)
    end
  | Error e ->
    begin
      let err = Core_kernel.Error.to_string_hum e in
      let msg = Format.sprintf "Load error: %s" err in
      Error (Toplevel_error.Failed_to_load_proj msg)
    end

let get_func (prog : Program.t) (name : string) : Sub.t =
  let subs = Term.enum sub_t prog in
  Seq.find_exn ~f:(fun s -> String.equal (Sub.name s) name) subs
