(** Implements {!Seeder}. *)

open !Core_kernel
open Bap_knowledge
open Bap_core_theory

module KB = Knowledge
open KB.Syntax
open KB.Let

(* Seed information about a particular patch. *)
type patch = {
  raw_ir : Ir.t;
  patch_name : string;
  minizinc_solutions : Minizinc.sol_set;
  exclude_regs : String.Set.t option;
}

(* A bundle of seed info that can be used to seed the KB
   for a new pipeline run. *)
type t = {
  patches : patch list;
}

(* Extract seed info from a {Data.Patch.t} instance.
   This is expected to return [None] if a user defined assembly patch *)
let extract_patch (p : Data.Patch.t) (s : KB.state)
    : patch option =
    let (let*) x f = Option.bind x ~f in
    let* (value, _) = KB.run Data.Patch.patch (KB.return p) s |> Result.ok in
    let* raw_ir = KB.Value.get Data.Patch.raw_ir value in
    let* patch_name = KB.Value.get Data.Patch.patch_name value in
    let minizinc_solutions = KB.Value.get Data.Patch.minizinc_solutions value in
    let exclude_regs = KB.Value.get Data.Patch.exclude_regs value in
    Some { raw_ir; patch_name; minizinc_solutions; exclude_regs }

(* Given a bundle of [seed] info, find the seed info for the patch with
   the specified [name]. *)
let get_patch_by_name (seed : t option) (name : string) : patch option =
  match seed with
  | None -> None
  | Some t ->
    List.find t.patches ~f:(fun p -> String.equal p.patch_name name)

(* Takes a [Data.computed] result and extract the info we want to use
   to seed the KB for a new pipeline run. *)
let extract_seed (value : Data.computed) (s : KB.state)
    : t =
  let patch_objects = KB.Value.get Data.Patched_exe.patches value in
  let the_list = Data.Patch_set.to_list patch_objects in
  let patch_seeds = List.map the_list ~f:(fun p -> extract_patch p s) in
  let patches = List.filter_opt patch_seeds in
  { patches }

(* Create a patch set. *)
let create_patches
    ?seed:(seed=None)
    (ps : Config.patch list)
    : Data.Patch_set.t KB.t =
  let create_patch (seed : t option) (p : Config.patch) : Data.Patch.t KB.t =
    let* obj = KB.Object.create Data.Patch.patch in
    let patch_name = Config.patch_name p in
    let* lang =
      Utils.get_lang
        ~addr:(Config.patch_point p)
    in
    let* tgt =
      Utils.get_target
        ~addr:(Config.patch_point p)
    in
    let* () = Data.Patch.set_patch_name obj (Some patch_name) in
    let* () = match Config.patch_code p with
      | CCode ccode -> Data.Patch.set_patch_code obj (Some ccode)
      | ASMCode asmcode -> Data.Patch.set_assembly obj (Some [asmcode])
    in
    let* () = Data.Patch.set_patch_point obj (Some (Config.patch_point p)) in
    let* () = Data.Patch.set_patch_size obj (Some (Config.patch_size p)) in
    let* () = Data.Patch.set_lang obj lang in
    let* () = Data.Patch.set_target obj tgt in
    let* () = Data.Patch.set_patch_vars obj (Some (Config.patch_vars p)) in
    let* () = Data.Patch.set_sp_align obj (Some (Config.patch_sp_align p)) in
    let* () = match get_patch_by_name seed patch_name with
      | None -> KB.return ()
      | Some patch_seed ->
        let* () = Data.Patch.union_minizinc_solution
          obj patch_seed.minizinc_solutions in
        let* () = Data.Patch.set_raw_ir obj (Some patch_seed.raw_ir) in
        Data.Patch.set_exclude_regs obj patch_seed.exclude_regs
    in
    KB.return obj
  in
  let patches = List.map ps ~f:(fun p -> create_patch seed p) in
  KB.all patches >>| Data.Patch_set.of_list

(* Create a Data.Patch_space_set.t from the config, containing the patch spaces
 *)
let create_patch_spaces (patch_spaces : Config.patch_space list)
    : Data.Patch_space_set.t KB.t =
  let create_patch_space (ps : Config.patch_space) =
    let* obj = KB.Object.create Data.Patch_space.patch_space in
    let* () =
      Data.Patch_space.set_offset obj (Some (Config.(ps.space_offset)))
    in
    let* () =
      Data.Patch_space.set_size obj (Some (Config.(ps.space_size)))
    in
    KB.return obj
  in
  let* patch_spaces = KB.all (List.map patch_spaces ~f:create_patch_space) in
  KB.return (Data.Patch_space_set.of_list patch_spaces)

(* Create a {!Data.t} instance from the provided {Config.t} data,
   possibly adding extra [seed] info. *)
let init_KB
    ?seed:(seed=None)
    (config : Config.t)
    (proj : Bap.Std.Project.t)
  : Data.t KB.t =
  let filename = Config.exe config in
  let patch_list = Config.patches config in
  let patch_spaces = Config.patch_spaces config in
  let patched_exe_filepath = Config.patched_exe_filepath config in
  let mzn_model_filepath = Config.minizinc_model_filepath config in
  let target = Bap.Std.Project.target proj in
  let addr_size = Theory.Target.bits target in
  let* patches = create_patches patch_list ~seed in
  let* patch_spaces = create_patch_spaces patch_spaces in
  let* obj = KB.Object.create Data.cls in
  let* () = Data.Original_exe.set_filepath obj (Some filename) in
  let* () = Data.Patched_exe.set_filepath obj patched_exe_filepath in
  let* () = Data.Original_exe.set_target obj target in
  let* () = Data.Patched_exe.set_patches obj patches in
  let* () = Data.Original_exe.set_patch_spaces obj patch_spaces in
  let* () = Data.Solver.set_minizinc_model_filepath
    obj (Some mzn_model_filepath) in
  Events.(send @@ Info (Printf.sprintf "Address size: %d bits" addr_size));
  KB.return obj
