(* Implements {!Compiler}. *)

open !Core_kernel
open Bap_knowledge
open Bap_core_theory
open Bap.Std
open Knowledge.Syntax
open Knowledge.Let

module KB = Knowledge
module Arm = Arm_selector
module Subst = Substituter

(* Applies the peephole optimizer to the output of a given solver. *)
let optimized solver =
  function ir ->
    let+ (ir, sol) = solver ir in
    (Arm.peephole ir, sol)

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_assembly (solver : Ir.t -> (Ir.t * Minizinc.sol) KB.t)
    (ir : Ir.t) : (string list * Minizinc.sol) KB.t =
  optimized solver ir >>= fun (ir, new_sol) ->
  let pretty_ir = Arm.Pretty.arm_ir_pretty ir in
  match pretty_ir with
  | Ok assembly -> KB.return (assembly, new_sol)
  | Error e -> Kb_error.fail e

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_vibes_ir
    (tgt: Theory.target)
    (lang : Theory.language)
    (hvars : Higher_var.t list)
    (bir : Insn.t) : Ir.t KB.t =
  let ir = Blk.from_insn bir in
  let ir = Bir_opt.apply ir in
  let* ir = Subst.substitute tgt hvars ir in
  let ir = Arm.ARM_Gen.select ir in
  let ir = Arm.preassign tgt lang ir in
  KB.return ir

(* Compile one patch from BIR to VIBES IR *)
let compile_one_vibes_ir (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n ->
  let info_str =
    Format.asprintf "Translating patch %s BIR to VIBES IR..."
      (string_of_int n)
  in
  Events.(send @@ Info info_str);
  Data.Patch.get_bir patch >>= fun bir ->

  let info_str = Format.asprintf "\nPatch: %a\n\n%!" KB.Value.pp bir in
  Events.(send @@ Info info_str);

  Data.Patch.get_lang patch >>= fun lang ->
  Data.Patch.get_target patch >>= fun tgt ->
  Data.Patch.get_patch_vars_exn patch >>= fun hvars ->
  create_vibes_ir tgt lang hvars bir >>= fun ir ->
  Data.Patch.set_raw_ir patch (Some ir) >>= fun () ->
  Events.(send @@ Info "The patch has the following VIBES IR:\n");
  Events.(send @@ Rule);
  Events.(send @@ Info (Ir.pretty_ir ir));
  Events.(send @@ Rule);
  KB.return (n + 1)

(* Compile one patch from VIBES IR to assembly *)
let compile_one_assembly
    (solver : Theory.target ->
     Theory.language ->
     Minizinc.sol list ->
     Ir.t ->
     (Ir.t * Minizinc.sol) KB.t)
    (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n ->
  let info_str =
    Format.asprintf "Translating patch %s VIBES IR to assembly..."
      (string_of_int n)
  in
  Events.(send @@ Info info_str);
  Data.Patch.get_raw_ir_exn patch >>= fun ir ->
  Data.Patch.get_minizinc_solutions patch >>= fun prev_sols ->
  Data.Patch.get_target patch >>= fun target ->
  Data.Patch.get_lang patch >>= fun lang ->
  let prev_sols = Set.to_list prev_sols in
  create_assembly
    (solver target lang prev_sols)
    ir >>= fun (assembly, new_sol) ->
  Data.Patch.set_assembly patch (Some assembly) >>= fun () ->
  Events.(send @@ Info "The patch has the following assembly:\n");
  Events.(send @@ Rule);
  Events.(send @@ Info (String.concat ~sep:"\n" assembly));
  Events.(send @@ Rule);
  Data.Patch.add_minizinc_solution patch new_sol >>= fun () ->
  KB.return (n + 1)

(* Converts the patch (as BIR) to VIBES IR instructions. *)
let compile_ir (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting IR compiler");
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
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one_assembly (solver ~filepath:mzn_model)) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()
