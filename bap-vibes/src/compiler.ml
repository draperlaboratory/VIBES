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

let to_ssa (blks : Blk.t list) : Blk.t list =
  (* BAP will give us the blks in such an order that the first one is the
     entry blk. *)
  let entry_tid = List.hd_exn blks |> Term.tid in
  (* Create the subroutine, which will fill in the control-flow edges. *)
  let sub = List.fold blks
      ~init:(Sub.create () ~name:"dummy-wrapper")
      ~f:(fun sub blk -> Term.append blk_t sub blk) in
  (* Prune all unreachable blks, which are those whose sum of in-degree and
     out-degree is zero. We have to be careful to ignore the entry block. *)
  let sub =
    let cfg = Sub.to_cfg sub in
    Graphs.Ir.nodes cfg |> Seq.filter ~f:(fun node ->
        let blk = Graphs.Ir.Node.label node in
        Tid.(Term.tid blk <> entry_tid) &&
        Graphs.Ir.Node.degree node cfg = 0) |>
    Seq.fold ~init:sub ~f:(fun sub node ->
        let blk = Graphs.Ir.Node.label node in
        Term.remove blk_t sub @@ Term.tid blk) in
  (* Convert to SSA. *)
  let sub = Sub.ssa sub in
  (* TODO - try this: *)
  let sub = Linear_ssa.transform sub in
  Term.enum blk_t sub |> Seq.to_list

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_vibes_ir
    (tgt: Theory.target)
    (lang : Theory.language)
    (hvars : Higher_var.t list)
    (bir : Insn.t) : Ir.t KB.t =
  let ir = Blk.from_insns [bir] in
  let ir = Bir_opt.apply ir in
  let* ir = Subst.substitute tgt hvars ir in
  let ir = to_ssa ir in
  Events.(send @@ Info "SSA'd BIR\n");
  Events.(send @@ Info (
      List.map ir ~f:(fun blk -> Format.asprintf "    %a" Blk.pp blk) |>
      String.concat ~sep:"\n"));
  Events.(send @@ Info "\n\n");
  let* ir = Arm.ARM_Gen.select ir in
  let ir = Arm.preassign tgt lang ir in
  KB.return ir

(* Compile one patch from BIR to VIBES IR *)
let compile_one_vibes_ir (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n ->
  Data.Patch.get_assembly patch >>= begin function
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
      create_vibes_ir tgt lang hvars bir >>= fun ir ->
      Data.Patch.set_raw_ir patch (Some ir) >>= fun () ->
      Events.(send @@ Info "The patch has the following VIBES IR:\n");
      Events.(send @@ Rule);
      Events.(send @@ Info (Ir.pretty_ir ir));
      Events.(send @@ Rule);
      KB.return ()
  end >>= fun () -> KB.return (n + 1)
  
(* Compile one patch from VIBES IR to assembly *)
let compile_one_assembly
    (solver : Theory.target ->
     Theory.language ->
     Minizinc.sol list ->
     Ir.t ->
     (Ir.t * Minizinc.sol) KB.t)
    (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n ->
    Data.Patch.get_assembly patch >>= (fun asm ->
    match asm with
    | Some _asm ->
        Events.(send @@ Info "The patch already has assembly\n");
        Events.(send @@ Rule);
        KB.return () (* Assembly already set. Presumably by the user. *)
    | None ->
      begin
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
        Data.Patch.add_minizinc_solution patch new_sol 
      end) >>= fun () ->
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
