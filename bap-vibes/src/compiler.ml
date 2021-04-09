(* Implements {!Compiler}. *)

open !Core_kernel
open Bap_knowledge
open Bap.Std
open Knowledge.Syntax
open Knowledge.Let

module KB = Knowledge
module Arm = Arm_selector

(* Applies the peephole optimizer to the output of a given solver. *)
let optimized solver =
  function ir ->
    let+ (ir, sol) = solver ir in
    (Arm.peephole ir, sol)

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_assembly (solver : Ir.t -> (Ir.t * Minizinc.sol) KB.t)
    (ir : Ir.t) : (string list * Minizinc.sol) KB.t =
  optimized solver ir >>= fun (ir, new_sol) ->
  (* let* (ir, new_sol) =
   Either.value_map
      (* run the peephole optimizer here *)
      ~first:(optimized solver)
      ~second:Kb_error.fail
      (Ok ir) 
  in *)
  let pretty_ir = Arm.Pretty.arm_ir_pretty ir in
  match pretty_ir with
  | Ok assembly -> KB.return (assembly, new_sol)
  | Error e -> Kb_error.fail e

  (* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_vibes_ir (bir : Insn.t) : Ir.t KB.t =
  let arm_eff = Arm.effect bir in
  let err = Format.asprintf "arm_eff not found in:%a%!" KB.Value.pp bir in
  (* Makes for slightly clearer errors *)
  let arm_eff = Result.of_option arm_eff
    ~error:(Kb_error.Missing_semantics err) in
  (* For some reason Either is more fully featured *)
  let ir = Result.map ~f:Arm.ir arm_eff in
  match ir with
  | Ok ir -> KB.return ir
  | Error e -> Kb_error.fail e

(* Compile one patch from BIR to VIBES IR *)
let compile_one_vibes_ir (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n ->
  let info_str =
    Format.asprintf "Translating patch %s BIR to VIBES IR..."
      (string_of_int n)
  in
  Events.(send @@ Info info_str);
  Events.(send @@ Rule);
  Data.Patch.get_bir patch >>= fun bir ->
  create_vibes_ir bir >>= fun ir ->
  (* Stash the assembly in the KB. *)
  Data.Patch.set_raw_ir patch (Some ir) >>= fun () ->
  Events.(send @@ Info "The patch has the following VIBES IR:\n");
  Events.(send @@ Info (Ir.pretty_ir ir));
  Events.(send @@ Rule);
  KB.return (n+1)

  (* Compile one patch from VIBES IR to assembly *)
let compile_one_assembly (solver : Minizinc.sol list -> Ir.t -> (Ir.t * Minizinc.sol) KB.t)
(count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n ->
  let info_str =
  Format.asprintf "Translating patch %s VIBES IR to assembly..."
  (string_of_int n)
  in
  Events.(send @@ Info info_str);
  Events.(send @@ Rule);
  Data.Patch.get_raw_ir patch >>= fun ir ->
  match ir with
  | None -> Kb_error.fail Kb_error.Missing_raw_ir
  | Some ir ->
  Data.Patch.get_minizinc_solutions patch >>= fun prev_sols ->
  let prev_sols = Set.to_list prev_sols in
  create_assembly (solver prev_sols) ir >>= fun (assembly, new_sol) ->
  (* Stash the assembly in the KB. *)
  Data.Patch.set_assembly patch (Some assembly) >>= fun () ->
  Events.(send @@ Info "The patch has the following assembly:\n");
  Events.(send @@ Info (String.concat ~sep:"\n" assembly));
  Events.(send @@ Rule);

  (* Add solution to patch *)
  Data.Patch.add_minizinc_solution patch new_sol >>= fun () ->
  KB.return (n+1)

(* Converts the patch (as BIR) to VIBES IR instructions. *)
let compile_ir (obj : Data.t)
    : unit KB.t =
  Events.(send @@ Header "Starting IR compiler");

  (* Retrieve the patch (BIR) from the KB, and convert it to assembly. *)
  Events.(send @@ Info "Retrieving data from KB...");
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one_vibes_ir) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()

(* Converts the patch (as IR) to assembly instructions. *)
let compile_assembly ?solver:(solver = Minizinc.run_minizinc) (obj : Data.t)
    : unit KB.t =
  Events.(send @@ Header "Starting Minizinc compiler");
  Data.Solver.get_minizinc_model_filepath_exn obj >>= fun mzn_model ->
  Events.(send @@ Info ("Using minizinc model: " ^ mzn_model));

  (* Retrieve the patch (BIR) from the KB, and convert it to assembly. *)
  Events.(send @@ Info "Retrieving data from KB...");
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one_assembly (solver mzn_model)) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()
