(* Implements {!Utils}. *)

open Result
open Bap.Std
open Bap_knowledge
open Bap_core_theory
open Bap_demangle.Std
module KB = Knowledge
open KB.Let


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
let lift_kb_result (x : ('a, Kb_error.t) result) : 'a KB.t =
  match x with
  | Ok x -> KB.return x
  | Error e -> Kb_error.fail e

let run_process (command : string) (args : string list)
  : (unit, Kb_error.t) result =
  let (as_stdout, as_stdin) =
    Unix.open_process (String.concat " "  (command :: args)) in
  let status = Unix.close_process (as_stdout, as_stdin) in
  match status with
  | WEXITED 0 -> Ok ()
  | WEXITED 127 ->
    begin
      let msg = Format.sprintf "'%s' not found in PATH" command in
      Error (Kb_error.Command_not_found msg)
    end
  | WEXITED n ->
    begin
      let msg = Format.sprintf "%s returned exit code: %d" command n in
      Error (Kb_error.Exit_code msg)
    end
  | _ ->
    begin
      let msg =
        Format.sprintf "%s exited with unknown return status" command in
      Error (Kb_error.Unexpected_exit msg)
    end

let load_exe (filename : string)
  : (project * Program.t, Toplevel_error.t) result =
  let loader =
    let loaders = Project.Input.available_loaders () in
    if Core_kernel.List.mem ~equal:String.equal loaders "vibes-raw" then
      "vibes-raw"
    else
      "llvm"
  in
  let input = Project.Input.file ~loader:loader ~filename in
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

let get_func (prog : Program.t) (name : string) : Sub.t option =
  let subs = Term.enum sub_t prog in
  let find_with eq =
    Seq.find ~f:(fun s -> eq (Sub.name s) name) subs
  in
  (* FIXME: can we determine the source language here? *)
  let find_simple =
    find_with String.equal
  in
  if Option.is_none find_simple then
    (* We try the C++ demangler here *)
    let d = Demanglers.available () |> List.hd in
    find_with
      (fun s n ->
         let s = Demangler.run d s in
         String.equal s n)
  else
    find_simple

(* [with_valid_addr addr ~f:f] checks to see if the provided [addr] is
   a valid location in the uplifted binary, and if so collects the
   associated [tid] and invokes the callback [f]. Fails otherwise. *)
let with_valid_addr (addr : Bitvec.t) ~f:(f : tid -> 'a KB.t) : 'a KB.t =
  let* tid = Theory.Label.for_addr addr in
  let* is_valid = KB.collect Theory.Label.is_valid tid in
  match is_valid with
  | Some true ->
    f tid
  | _ ->
    (* FIXME: for some reason we always hit this case! *)
    f tid
    (* Kb_error.(fail (Incorrect_patch_point (Bitvec.to_string addr))) *)

let get_lang
    ~addr:(addr : Bitvec.t)
  : Theory.language KB.t =
  with_valid_addr addr
    ~f:(fun tid ->
        KB.collect Theory.Label.encoding tid)

let get_target
    ~addr:(addr : Bitvec.t)
  : Theory.target KB.t =
  with_valid_addr addr ~f:(Theory.Label.target)


(* A little tediousness to print FrontC defs *)
let print_c (pp : 'a -> unit) (data : 'a) : string =
  let (tmp_file, chan) = Filename.open_temp_file "frontc" ".out" in
  let old_chan = !Cprint.out in
  Cprint.out := chan;
  pp data;
  Cprint.flush ();
  let open Core_kernel in
  Out_channel.close chan;
  let res =
    In_channel.read_lines tmp_file
    |> String.concat ~sep:"\n"
  in
  Cprint.out := old_chan;
  Sys.remove tmp_file;
  res
