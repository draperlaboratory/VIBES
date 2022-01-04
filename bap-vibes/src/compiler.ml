(* Implements {!Compiler}. *)

open !Core_kernel
open Bap_knowledge
open Bap_core_theory
open Bap.Std
open Knowledge.Syntax
open Knowledge.Let

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
  let pretty_ir = Arm.Pretty.arm_ir_pretty ir in
  match pretty_ir with
  | Ok assembly -> KB.return (assembly, new_sol)
  | Error e -> Kb_error.fail e

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_vibes_ir
    (isel_model_filepath : string option)
    (tgt: Theory.target)
    (lang : Theory.language)
    (hvars : Higher_var.t list)
    (sp_align : int)
    (code : insn) : (Ir.t * String.Set.t) KB.t =
  let* {ir; exclude_regs; argument_tids} =
    Bir_passes.run code ~tgt ~hvars ~sp_align in
  Events.(send @@ Info "Transformed BIR\n");
  Events.(send @@ Info (
      List.map ir ~f:(fun blk -> Format.asprintf "    %a" Blk.pp blk) |>
      String.concat ~sep:"\n"));
  Events.(send @@ Info "\n\n");
  let* is_arm = Arm.is_arm lang and* is_thumb = Arm.is_thumb lang in
  let+ ir =
    if is_arm || is_thumb then
      let+ ir = 
        match isel_model_filepath with
        | None -> Arm.ARM_Gen.select ir ~argument_tids ~is_thumb
        | Some isel_model_filepath ->
          begin
            Events.(send @@ Info "Running Minizinc instruction selector.");
            let* ir = KB.List.map ~f:Flatten.flatten_blk ir in
            List.iter ir ~f:(fun blk ->
            Events.(send @@ Info (sprintf "The patch has the following BIL: %a" Blk.pps blk)));
            let* ir = Isel.run ~isel_model_filepath ir Arm.Isel.patterns Arm.Isel.templates in
            KB.return ir
          end
      in
      Arm.preassign tgt ir ~is_thumb
    else Kb_error.(fail @@ Other (
        sprintf "Unsupported lang %s" (Theory.Language.to_string lang))) in
  ir, exclude_regs

(* Compile one patch from BIR to VIBES IR *)
let compile_one_vibes_ir (count : int KB.t) (patch : Data.Patch.t) (isel_model_filepath : string) : int KB.t =
  count >>= fun n -> Data.Patch.get_assembly patch >>= begin function
    | Some _asm ->
      Events.(send @@ Info "The patch has no IR to translate.\n");
      KB.return () (* Assembly already set. Presumably by the user. *)
    | None ->
      let info_str =
        Format.sprintf "Translating patch %d BIR to VIBES IR..." n
      in
      Events.(send @@ Info info_str);
      Data.Patch.get_bir patch >>= fun bir ->

      let info_str = Format.asprintf "\nPatch: %a\n\n%!" KB.Value.pp bir in
      Events.(send @@ Info info_str);

      Data.Patch.get_lang patch >>= fun lang ->
      Data.Patch.get_target patch >>= fun tgt ->
      Data.Patch.get_patch_vars_exn patch >>= fun hvars ->
      Data.Patch.get_sp_align_exn patch >>= fun sp_align ->
      create_vibes_ir isel_model_filepath tgt lang hvars sp_align bir >>= fun (ir, exclude_regs) ->
      Data.Patch.set_raw_ir patch (Some ir) >>= fun () ->
      Data.Patch.set_exclude_regs patch (Some exclude_regs) >>= fun () ->
      Events.(send @@ Info "The patch has the following VIBES IR:\n");
      Events.(send @@ Rule);
      Events.(send @@ Info (Ir.pretty_ir ir));
      Events.(send @@ Rule);
      KB.return ()
  end >>= fun () -> KB.return (n + 1)

(* Compile one patch from VIBES IR to assembly *)
let compile_one_assembly
    (solver :
       ?exclude_regs:String.Set.t ->
     Theory.target ->
     Theory.language ->
     Minizinc.sol list ->
     Ir.t ->
     (Ir.t * Minizinc.sol) KB.t)
    (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n -> Data.Patch.get_assembly patch >>= begin function
    | Some _asm ->
      Events.(send @@ Info "The patch already has assembly\n");
      Events.(send @@ Rule);
      KB.return () (* Assembly already set. Presumably by the user. *)
    | None ->
      let info_str =
        Format.asprintf "Translating patch %s VIBES IR to assembly..."
          (string_of_int n)
      in
      Events.(send @@ Info info_str);
      Data.Patch.get_raw_ir_exn patch >>= fun ir ->
      Data.Patch.get_exclude_regs patch >>= fun exclude_regs ->
      let exclude_regs = Option.value exclude_regs ~default:String.Set.empty in
      Data.Patch.get_minizinc_solutions patch >>= fun prev_sols ->
      Data.Patch.get_target patch >>= fun target ->
      Data.Patch.get_lang patch >>= fun lang ->
      let prev_sols = Set.to_list prev_sols in
      create_assembly
        (solver target lang prev_sols ~exclude_regs)
        ir >>= fun (assembly, new_sol) ->
      Data.Patch.set_assembly patch (Some assembly) >>= fun () ->
      Events.(send @@ Info "The patch has the following assembly:\n");
      Events.(send @@ Rule);
      Events.(send @@ Info (String.concat ~sep:"\n" assembly));
      Events.(send @@ Rule);
      Data.Patch.add_minizinc_solution patch new_sol 
  end >>= fun () -> KB.return (n + 1)

(* Converts the patch (as BIR) to VIBES IR instructions. *)
let compile_ir ?(isel_model_filepath = None) (obj : Data.t) : unit KB.t =
  Events.(send @@ Header "Starting IR compiler");
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one_vibes_ir isel_model_filepath) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()

(* Converts the patch (as IR) to assembly instructions. *)
let compile_assembly ?solver:(solver = Minizinc.run_unison) (obj : Data.t)
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
