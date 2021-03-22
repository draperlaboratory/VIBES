(* Implements {!Pipeline}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge

module KB = Knowledge
open KB.Let

let (let+) x f = Result.bind x ~f

(* Fail if the count is more than max_tries. *)
let halt_if_too_many (count : int) (max_tries : int option)
    : (unit, Toplevel_error.t) result =
  match max_tries with
  | None -> Ok ()
  | Some 0 -> Ok ()
  | Some n ->
    if count > n then Error (Toplevel_error.Max_tries n)
    else Ok ()

(* Compute and produce the patched exe. This is called by [KB.run] below. *)
let create_patch ?seed:(seed=None) (config : Config.t) (proj : project)
    : Data.t KB.t =
  let* obj = Seed.create config ~seed in
  let* () = Exe_ingester.ingest obj proj in
  let* () = Patch_ingester.ingest obj in
  let* () = Compiler.compile obj in
  let* () = Patcher.patch obj in
  KB.return obj

(* Extract the patched exe filepath from the [KB.run] result, and
   copy the patched exe to the user-specified location. Return the
   final filepath (or the relevant error). *)
let finalize_patched_exe (value : Data.computed) 
    : (string, Toplevel_error.t) result =
  let original_exe_filepath : string option =
    KB.Value.get Data.Original_exe.filepath value
  in
  let tmp_patched_exe_filepath : string option =
    KB.Value.get Data.Patched_exe.tmp_filepath value
  in
  match (original_exe_filepath, tmp_patched_exe_filepath) with
  | (Some orig_path, Some tmp_path) ->
    begin
      let user_filepath : string option =
        KB.Value.get Data.Patched_exe.filepath value
      in
      let patched_exe_filepath : string =
        Option.value user_filepath
          ~default:((Filename.basename orig_path) ^ ".patched")
      in
      Utils.cp tmp_path patched_exe_filepath;
      Events.(send @@ Info 
        (Printf.sprintf "Patched exe: %s\n " patched_exe_filepath));
      Ok patched_exe_filepath
    end
  | (None, _) ->
    begin
      let msg = "Missing filepath for the original exe" in
      let err = Errors.Other msg in
      Error (Toplevel_error.KB_error err)
    end
  | (_, None) ->
    begin
      let msg =
        "No filepath for the temporary patched exe was computed" in
      let err = Errors.Other msg in
      Error (Toplevel_error.KB_error err)
    end

(* Use [KB.run] to run the provided computation [f] with the given [state]. *)
let run_KB_computation (f : Data.cls KB.obj KB.t) (state : KB.state) 
    : (Data.computed * KB.state, Toplevel_error.t) result =
  let result = KB.run Data.cls f state in
  match result with
  | Error e ->
    begin
      let msg = Format.asprintf "%a\n" KB.Conflict.pp e in
      Events.(send @@ Info "An error occurred during a KB computation");
      Events.(send @@ Info msg); 
      let err = Errors.Other msg in
      Error (Toplevel_error.KB_error err)
    end
  | Ok (value, state') -> Ok (value, state')

(* This is the main CEGIS loop. It computes a patch (via a [KB.run]),
   and it verifies the patch. If the patch is correct, it returns the
   filepath of the patched exe. If incorrect, it runs again. *)
let rec cegis ?count:(count=0) ?max_tries:(max_tries=None) ?seed:(seed=None)
    (config : Config.t) (orig_proj : project) (orig_prog : Program.t) 
    (state : KB.state) : (string, Toplevel_error.t) result =

  Events.(send @@ Header "Starting CEGIS iteration");
  Events.(send @@ Info (Printf.sprintf "Iteration: %d" count));

  let+ _ = halt_if_too_many count max_tries in

  let computation = create_patch config orig_proj ~seed in
  let+ value, state' = run_KB_computation computation state in

  let+ patched_exe_filepath = finalize_patched_exe value in
  let+ _, patch_prog = Utils.load_exe patched_exe_filepath in

  let func = Config.func config in
  let property = Config.property config in
  match Verifier.verify func property ~orig_prog ~patch_prog with
  | Ok Verifier.Done -> 
    begin
      Ok patched_exe_filepath
    end
  | Ok Verifier.Again ->
    begin
      let new_count = count + 1 in
      let+ new_seed = Seed.extract value state' in
      cegis config orig_proj orig_prog state
        ~count:new_count ~max_tries ~seed:(Some new_seed)
    end
  | Error e -> Error e

(* This is the public function that sets up and triggers the pipeline. *)
let run (config : Config.t) : (string, Toplevel_error.t) result =
  Events.(send @@ Header "Starting pipeline");
  Events.(send @@ Info (Format.asprintf "%a" Config.pp config));

  let filepath = Config.exe config in
  Events.(send @@ Info (Format.sprintf "Loading into BAP: %s..." filepath));
  let+ orig_proj, orig_prog = Utils.load_exe filepath in

  let state = Toplevel.current () in
  let max_tries = Config.max_tries config in
  cegis config orig_proj orig_prog state ~max_tries
