(** Implements {!Seeder}. *)

open !Core_kernel
open Bap_knowledge
module KB = Knowledge
open KB.Syntax
open KB.Let

let (let+) x f = Result.bind x ~f

(* Seed information about a particular patch. *)
type patch = {
  patch_name : string;
  minizinc_solutions : Minizinc.sol_set;
}

(* A bundle of seed info that can be used to seed the KB
   for a new pipeline run. *)
type t = {
  patches : patch list;
}

(* Extract seed info from a {Data.Patch.t} instance. *)
let extract_patch (p : Data.Patch.t) (s : KB.state)
    : (patch, Toplevel_error.t) result =
  match KB.run Data.Patch.patch (KB.return p) s with
  | Error e ->
    begin
      let msg = Format.asprintf 
        "(KB.return patch) failed in KB: %a" KB.Conflict.pp e in
      let err = Kb_error.Other msg in
      Error (Toplevel_error.KB_error err)
    end
  | Ok (value, _) ->
    begin
      match KB.Value.get Data.Patch.patch_name value with
      | None ->
        begin
          let msg = "No patch_name in KB to use for seed info" in
          Error (Toplevel_error.No_value_in_KB msg)
        end
      | Some patch_name ->
        begin
          let minizinc_solutions =
            KB.Value.get Data.Patch.minizinc_solutions value in
          Ok { patch_name; minizinc_solutions }
        end
    end

(* Given a bundle of [seed] info, find the seed info for the patch with
   the specified [name]. *)
let patch_with_name (seed : t option) (name : string) : patch option =
  match seed with
  | None -> None
  | Some t ->
    List.find t.patches ~f:(fun p -> String.equal p.patch_name name)

(* Takes a [Data.computed] result and extract the info we want to use
   to seed the KB for a new pipeline run. *)
let extract_seed (value : Data.computed) (s : KB.state)
    : (t, Toplevel_error.t) result =
  let patch_objects = KB.Value.get Data.Patched_exe.patches value in
  let the_list = Data.Patch_set.to_list patch_objects in
  let patch_seeds = List.map the_list ~f:(fun p -> extract_patch p s) in
  let+ patches = Result.all patch_seeds in
  Ok { patches }

(* Create a patch set. *)
let create_patches ?seed:(seed=None) (ps : Config.patch list)
    : Data.Patch_set.t KB.t =
  let create_patch (seed : t option) (p : Config.patch)
      : Data.Patch.t KB.t =
    KB.Object.create Data.Patch.patch >>= fun obj ->
    let patch_name = Config.patch_name p in
    Data.Patch.set_patch_name obj (Some patch_name) >>= fun () ->
    Data.Patch.set_patch_code obj (Some (Config.patch_code p)) >>= fun () ->
    Data.Patch.set_patch_point obj (Some (Config.patch_point p)) >>= fun () ->
    Data.Patch.set_patch_size obj (Some (Config.patch_size p)) >>= fun () ->
    let* () = match patch_with_name seed patch_name with
      | None -> KB.return ()
      | Some patch_seed -> Data.Patch.union_minizinc_solution
          obj patch_seed.minizinc_solutions
    in
    KB.return obj
  in
  let patches = List.map ps ~f:(fun p -> create_patch seed p) in
  KB.all patches >>| Data.Patch_set.of_list

(* Create a {!Data.t} instance from the provided {Config.t} data,
   possibly adding extra [seed] info. *)
let init_KB ?seed:(seed=None) (config : Config.t) : Data.t KB.t =
  let exe = Config.exe config in
  let patch_list = Config.patches config in
  let func = Config.func config in
  let property = Config.property config in
  let patched_exe_filepath = Config.patched_exe_filepath config in
  let mzn_model_filepath = Config.minizinc_model_filepath config in
  create_patches patch_list ~seed >>= fun patches ->
  KB.Object.create Data.cls >>= fun obj ->
  Data.Original_exe.set_filepath obj (Some exe) >>= fun () ->
  Data.Patched_exe.set_filepath obj patched_exe_filepath >>= fun () ->
  Data.Patched_exe.set_patches obj patches >>= fun () ->
  Data.Solver.set_minizinc_model_filepath
    obj (Some mzn_model_filepath) >>= fun () ->
  Data.Verifier.set_func obj (Some func) >>= fun () ->
  Data.Verifier.set_property obj (Some property) >>= fun () ->
  KB.return obj