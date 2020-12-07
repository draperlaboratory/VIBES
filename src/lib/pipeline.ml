(* Implements {!Pipeline}. *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge

(* Fail if the count is more than max_tries. *)
let halt_if_too_many (count : int) (max_tries : int option) : unit KB.t = 
  match max_tries with
  | None -> KB.return ()
  | Some 0 -> KB.return ()
  | Some n ->
    if count > n then Errors.fail (Errors.Max_tries n)
    else KB.return ()

(* The main CEGIS loop. This compiles the patch into assembly, splices the
   new code into the original exe, and then checks if the patched exe is
   correct. If not, it repeats the process. *) 
let rec cegis ?count:(count=0) ?max_tries:(max_tries=None) 
    (obj : Data.t) : Data.t KB.t =

  Events.(send @@ Header "Starting CEGIS iteration");
  Events.(send @@ Info (Printf.sprintf "Iteration: %d" count));
  halt_if_too_many count max_tries >>= fun _ ->

  (* Do the patch. *)
  Compiler.compile obj >>= fun _ ->
  Patcher.patch obj >>= fun _ ->

  (* If the patched exe is correct, we're done. Otherwise, create a new
     Data object with an updated correctness property and try again. *)
  Verifier.verify obj >>= fun next_step ->
  match next_step with
  | Verifier.Done -> KB.return obj
  | Verifier.Again property -> 
    begin
      Data.fresh obj ~property >>= fun obj' ->
      cegis obj' ~count:(count + 1) ~max_tries
    end

(* Set everything up for the CEGIS loop, and then run the CEGIS loop. *)
let init_and_run (config : Config.t) =

  (* Get a Data object we can work with. *)
  Data.create config >>= fun obj ->

  (* Load the original exe and the patch. *)
  Exe_ingester.ingest obj >>= fun _ ->
  Patch_ingester.ingest obj >>= fun _ ->

  (* Start the CEGIS loop. *)
  cegis obj ~max_tries:(Config.max_tries config)

(* This is the public function that runs the pipeline. *)
let run (config : Config.t) : (string, KB.Conflict.t) result =

  Events.(send @@ Header "Starthing pipeline");
  Events.(send @@ Info (Format.asprintf "%a" Config.pp config));

  (* Initialize and run the CEGIS loop in the KB monad. *)
  let result = KB.run Data.cls (init_and_run config) KB.empty in
  Events.(send @@ Header "Pipeline finished");

  (* If all went well, report the patched exe filepath. *)
  match result with
  | Ok (obj, _) ->
    begin
      let original_exe_filepath : string option =
        KB.Value.get Data.Original_exe.filepath obj
      in
      let tmp_patched_exe_filepath : string option =
        KB.Value.get Data.Patched_exe.tmp_filepath obj
      in
      match (original_exe_filepath, tmp_patched_exe_filepath) with
      | (Some orig_path, Some tmp_path) ->
         let user_filepath : string option =
           KB.Value.get Data.Patched_exe.filepath obj
         in
         let patched_exe_filepath : string =
           Option.value user_filepath
             ~default:(  (Filename.basename orig_path)
                       ^ ".patched")
         in
         begin
           Utils.cp tmp_path patched_exe_filepath;
           Events.(send @@ 
             Info (Printf.sprintf "Patched exe: %s\n " patched_exe_filepath));
           Ok patched_exe_filepath
         end
      | (None, _) ->
         begin
          let msg = "Missing filepath for the original exe" in
          let err = Errors.Problem (Errors.Other msg) in
          Events.(send @@ Info msg);
          Error err
         end
      | (_, None) ->
        begin
          let msg = "No filepath for the temporary patched exe was computed" in
          let err = Errors.Problem (Errors.Other msg) in
          Events.(send @@ Info msg);
          Error err
        end
    end

  (* Otherwise, error. *)
  | Error e -> 
    begin
      Events.(send @@ Info "An error occurred");
      Events.(send @@ Info (Format.asprintf "%a\n" KB.Conflict.pp e));
      Error e
    end 
