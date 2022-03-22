(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

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

(* This function parses the patch code and builds the initial patch IR. *)
let init (config : Config.t) (proj : project) : Data.t KB.t =
  let* obj = Seeder.init_KB config proj ~seed:None in
  let* () = Patch_ingester.ingest obj in
  let isel_model_filepath = Config.minizinc_isel_filepath config in
  let* () = Compiler.compile_ir ~isel_model_filepath obj in
  KB.return obj

(* This function produces a patched exe. *)
let create_patched_exe ~seed:(seed : Seeder.t) (config : Config.t)
    (proj : project) : Data.t KB.t =
  let* obj = Seeder.init_KB config proj ~seed:(Some seed) in
  let* () = Compiler.compile_assembly obj in
  let* () = Patcher.patch obj in
  KB.return obj

(* Each time we produce a patch, we give it a tmp filepath. This function
   retrieves that value from the computed KB result. *)
let get_tmp_patched_exe_filepath (value : Data.computed)
    : (string, Toplevel_error.t) result =
  let tmp_filepath : string option =
    KB.Value.get Data.Patched_exe.tmp_filepath value in
  match tmp_filepath with
  | None ->
    begin
      let msg =
        "KB is missing a filepath for the temporary patched exe" in
      Error (Toplevel_error.No_value_in_KB msg)
    end
  | Some filepath -> Ok filepath

(* Extract the patched exe filepath from the [KB.run] result, and
   copy the patched exe to the user-specified location. Return the
   final filepath (or the relevant error). *)
let finalize_patched_exe (value : Data.computed)
    : (string, Toplevel_error.t) result =
  let original_exe_filepath : string option =
    KB.Value.get Data.Original_exe.filepath value in
  let tmp_patched_exe_filepath : string option =
    KB.Value.get Data.Patched_exe.tmp_filepath value in
  match (original_exe_filepath, tmp_patched_exe_filepath) with
  | (Some orig_path, Some tmp_path) ->
    begin
      let user_filepath : string option =
        KB.Value.get Data.Patched_exe.filepath value in
      let patched_exe_filepath : string =
        Option.value user_filepath
          ~default:((Filename.basename orig_path) ^ ".patched") in
      Utils.cp tmp_path patched_exe_filepath;
      Events.(send @@ Info
        (Printf.sprintf "Patched exe: %s\n " patched_exe_filepath));
      Ok patched_exe_filepath
    end
  | (None, _) ->
    begin
      let msg = "KB is missing a filepath for the original exe" in
      Error (Toplevel_error.No_value_in_KB msg)
    end
  | (_, None) ->
    begin
      let msg =
        "When finalizing patched exe, no tmp filepath found in KB" in
      Error (Toplevel_error.No_value_in_KB msg)
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
      let err = Kb_error.Other msg in
      Error (Toplevel_error.KB_error err)
    end
  | Ok (value, new_state) -> Ok (value, new_state)

(* This is the main CEGIS loop. It constructs a patched exe and verifies it.
   If its correct, it returns the filepath. If not, it runs again. *)
let rec cegis ?count:(count=1) ?max_tries:(max_tries=None)
    ~seed:(seed : Seeder.t) (config : Config.t) (orig_proj : project)
    (orig_prog : Program.t) (state : KB.state)
    : (string, Toplevel_error.t) result =
  Events.(send @@ Header "Starting CEGIS iteration");
  Events.(send @@ Info (Printf.sprintf "Iteration: %d" count));

  let+ _ = halt_if_too_many count max_tries in

  let computation = create_patched_exe config orig_proj ~seed in
  let+ value, new_state = run_KB_computation computation state in

  let+ tmp_patched_filepath = get_tmp_patched_exe_filepath value in
  Events.(send @@
    Info (Printf.sprintf "Temp patched exe: %s" tmp_patched_filepath));

  (* Temporarily use the new KB state when loading the patched binary. *)
  Toplevel.set new_state;
  let+ _, patch_prog = Utils.load_exe tmp_patched_filepath in
  Toplevel.set state;

  let wp_params = Config.wp_params config in
  let target = Project.target orig_proj in
  let verif_res =
    Verifier.verify target wp_params
      ~orig_prog:(orig_prog, Config.exe config)
      ~patch_prog:(patch_prog, tmp_patched_filepath)
  in
  match verif_res with
  | Ok Verifier.Done -> finalize_patched_exe value
  | Ok Verifier.Again ->
    begin
      let new_count = count + 1 in
      let new_seed = Seeder.extract_seed value new_state in
      cegis config orig_proj orig_prog state
        ~count:new_count ~max_tries ~seed:new_seed
    end
  | Error e -> Error e

(* This is the public function that sets up and triggers the pipeline. *)
let run (config : Config.t) : (string, Toplevel_error.t) result =
  Events.(send @@ Header "Starting pipeline");
  Events.(send @@ Info (Format.asprintf "%a" Config.pp config));

  Events.(send @@ Info "Checking whether to use loader vibes-raw...");
  let res = Loader.register_loader config in
  if res then Events.(send @@ Info "Yes! Using loader vibes-raw.")
  else Events.(send @@ Info "No! Using default loader (llvm).");

  let filepath = Config.exe config in
  Events.(send @@ Info (Format.sprintf "Loading into BAP: %s..." filepath));
  let+ orig_proj, orig_prog = Utils.load_exe filepath in

  let state = Toplevel.current () in
  let computation = init config orig_proj in
  let+ obj, new_state = run_KB_computation computation state in
  let seed = Seeder.extract_seed obj new_state in

  let max_tries = Config.max_tries config in
  cegis config orig_proj orig_prog new_state ~max_tries ~seed
