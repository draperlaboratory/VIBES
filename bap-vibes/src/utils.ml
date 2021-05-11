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

let get_text_addr filename : string =
  (* TODO: Surely there must be a better way *)
  let open !Core_kernel in
  let command =
    Printf.sprintf
      "objdump -h %s | grep text | awk '{print $4}'"
      filename
  in
  let in_channel = Caml_unix.open_process_in command in
  match In_channel.input_line in_channel with
  | None -> failwith "get_text_addr: failed invocation to readelf"
  | Some addr_string ->
    Printf.sprintf "0x%s" addr_string

let get_text_offset filename =
  (* TODO: Surely there must be a better way *)
  let open !Core_kernel in
  let command =
    Printf.sprintf
      "objdump -h %s | grep text | awk '{print $6}'"
      filename
  in
  let in_channel = Caml_unix.open_process_in command in
  match In_channel.input_line in_channel with
  | None -> failwith "get_text_offset: failed invocation to readelf"
  | Some offset_string ->
    Printf.sprintf "0x%s" offset_string

(* Do some tedious invocations to readelf and some arithmetic to get
   the address from an offset. *)
let compute_offset_from_addr (filename : string) (addr_size : int) : Bitvec.t =
  let module M = Bitvec.Make(struct let modulus = Bitvec.modulus addr_size end) in
  let addr = filename |> get_text_addr |> Bitvec.of_string in
  let offset = filename |> get_text_offset |> Bitvec.of_string in
  M.(addr - offset)

let get_lang
    ~filename:(filename : string)
    ~addr_size:(addr_size : int)
    ~addr:(addr : Bitvec.t)
  : Theory.language KB.t =
  (* FIMXE: remove this when we replace offsets with addresses *)
  (* let offset = compute_offset_from_addr filename addr_size in
  let addr = Bitvec.M32.(addr + offset) in *)
  let* tid = Theory.Label.for_addr addr in
  let* lang = KB.collect Theory.Label.encoding tid in
  KB.return lang

let get_target
    ~filename:(filename : string)
    ~addr_size:(addr_size : int)
    ~addr:(addr : Bitvec.t)
  : Theory.target KB.t =
  (* FIMXE: remove this when we replace offsets with addresses *)
  let offset = compute_offset_from_addr filename addr_size in
  let addr = Bitvec.M32.(addr + offset) in
  let* tid = Theory.Label.for_addr addr in
  let* tgt = Theory.Label.target tid in
  KB.return tgt
