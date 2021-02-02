(* Implements {!Compiler}. *)

open !Core_kernel
open Bap_knowledge
open Bap.Std
open Knowledge.Syntax
open Knowledge.Let

module KB = Knowledge
module Arm = Arm_selector

(* TODO: make this the place where we instantiate the selector and
   Minizinc functors. *)
module Ir = Minizinc.Ir

(* Converts a list of BIR statements to a list of ARM assembly strings. *)
let create_assembly (solver : Ir.t -> Ir.t KB.t)
    (bir : Insn.t) : string list KB.t =
  let arm_eff = Arm.effect bir in
  let err = Format.asprintf "arm_eff not found in:%a%!" KB.Value.pp bir in
  (* Makes for slightly clearer errors *)
  let arm_eff = Result.of_option arm_eff
      ~error:(Errors.Missing_semantics err) in
  (* For some reason Either is more fully featured *)
  let ir = Result.map ~f:Arm.ir arm_eff |> Result.to_either in
  let* ir = Either.value_map ~first:solver ~second:Errors.fail ir in
  let pretty_ir = Arm.Pretty.arm_ir_pretty ir in
  match pretty_ir with
  | Ok assembly -> KB.return assembly
  | Error e -> Errors.fail e


(* Compile one patch from BIR to assembly *)
let compile_one (solver : Ir.t -> Ir.t KB.t)
      (count : int KB.t) (patch : Data.Patch.t) : int KB.t =
  count >>= fun n ->
  let info_str =
    Format.asprintf "Translating patch %s BIR to assembly..."
      (string_of_int n)
  in
  Events.(send @@ Info info_str);
  Events.(send @@ Rule);
  Data.Patch.get_bir patch >>= fun bir ->
  create_assembly solver bir >>= fun assembly ->

  (* Stash the assembly in the KB. *)
  Data.Patch.set_assembly patch (Some assembly) >>= fun () ->
  Events.(send @@ Info "The patch has the following assembly:\n");
  Events.(send @@ Info (String.concat ~sep:"\n" assembly));
  Events.(send @@ Rule);

  KB.return (n+1)


(* Converts the patch (as BIR) to assembly instructions. *)
let compile ?solver:(solver = Minizinc.run_minizinc) (obj : Data.t)
    : unit KB.t =
  Events.(send @@ Header "Starting compiler");
  Data.Solver.get_minizinc_model_filepath_exn obj >>= fun mzn_model ->
  Events.(send @@ Info ("Using minizinc model: " ^ mzn_model));

  (* Retrieve the patch (BIR) from the KB, and convert it to assembly. *)
  Events.(send @@ Info "Retreiving data from KB...");
  Data.Patched_exe.get_patches obj >>= fun patches ->
  let size : string = string_of_int (Data.Patch_set.length patches) in
  Events.(send @@ Info ("There are " ^ size ^ " patch fragments."));
  Data.Patch_set.fold patches ~init:(KB.return 1)
    ~f:(compile_one (solver mzn_model)) >>= fun _ ->
  Events.(send @@ Info "Done.");
  KB.return ()
