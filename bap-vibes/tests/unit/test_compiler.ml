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

open !Core_kernel
open Bap_knowledge
open Knowledge.Syntax
open Bap_vibes
open OUnit2

module KB = Knowledge
module H = Helpers


let dummy_solver
    ?(congruence = [])
    ?(exclude_regs = String.Set.empty)
    ?(extra_constraints = None)
    _ _ vir ~filepath:_ ~gpr:_ ~regs:_ =
  ignore congruence;
  ignore exclude_regs;
  ignore extra_constraints;
  KB.return (vir, Test_minizinc.dummy_sol)

(* Test that [Compiler.compile] works as expected. *)
let test_compile (_ : test_ctxt) : unit =

  (* Skip this test for now. *)
  (* H.skip_test "Doesn't work without the dummy solver"; *)

  (* Run the compiler. *)
  let computation =
    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Data.Solver.set_minizinc_model_filepath
      obj (Some H.minizinc_model_filepath) >>= fun () ->
    Patches.get_sem H.patch 32 >>= fun sem ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.init_sem patch >>= fun () ->
    Data.Patch.set_target patch (H.the_target ()) >>= fun () ->
    Data.Patch.set_lang patch (H.the_lang ()) >>= fun () ->
    Data.Patch.set_sem patch sem >>= fun () ->
    Data.Patch.set_sp_align patch (Some 0) >>= fun () ->
    Data.Patch.set_patch_vars patch (Some []) >>= fun () ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun () ->

    (* Now run the compiler. *)
    Compiler.compile_ir obj >>= fun _ ->
    Compiler.compile_assembly ~solver:dummy_solver obj >>= fun () ->
    Data.Patched_exe.get_patches obj >>= fun patches ->
    match Data.Patch_set.to_list patches with
    | [] -> assert_failure "Result patch missing."
    | (p :: []) -> KB.return p
    | _ -> assert_failure "Multiple patches returned when one expected."
  in

  (* The complier should stash the assembly it produces in the KB. *)
  let patch_value = KB.run Data.Patch.patch computation KB.empty in
  let expected = Some H.assembly in
  let pat = Str.regexp "blk.*:" in
  let s_equal expected actual =
    Str.string_match pat actual 0 ||
    String.equal expected actual in
  H.assert_property
    ~cmp:(Option.equal (List.equal s_equal))
    ~p_res:H.print_string_list_opt ~p_expected:H.print_string_list_opt
    Data.Patch.assembly expected patch_value

(* Test that [Compiler.compile] errors when no minizinc model filepath
   is stashed in the KB. *)
let test_compile_with_no_minizinc_model_filepath (_ : test_ctxt) : unit =

  (* Run the compiler. *)
  let computation =
    H.obj () >>= fun obj ->
    Compiler.compile_ir obj >>= fun _ ->
    Compiler.compile_assembly ~solver:dummy_solver obj >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The compiler should diverge with the appropriate error. *)
  let expected = Kb_error.Problem Kb_error.Missing_minizinc_model_filepath in
  H.assert_error Data.Solver.minizinc_model_filepath expected result

(* Test that [Compiler.compile] handles no patch (BIR) in the KB. *)
let test_compile_with_no_patch (_ : test_ctxt) : unit =

  (* Run the compiler. *)
  let computation =
    (* At the start, the KB is empty. No patch (BIR) is stashed in it. *)
    H.obj () >>= fun obj ->
    Data.Solver.set_minizinc_model_filepath
      obj (Some H.minizinc_model_filepath) >>= fun () ->
    Compiler.compile_ir obj >>= fun _ ->
    Compiler.compile_assembly ~solver:dummy_solver obj >>= fun _ ->
    Data.Patched_exe.get_patches obj >>= fun patches ->
    match Data.Patch_set.to_list patches with
    | [] -> Kb_error.fail Kb_error.Missing_patch_code
    | (p :: _) -> KB.return p
  in
  let result = KB.run Data.Patch.patch computation KB.empty in
  let expected =
    Kb_error.Problem Kb_error.Missing_patch_code in

  H.assert_error
    ~printer:H.print_string_list_opt
    Data.Patch.assembly
    expected
    result

let suite = [
  "Test Compiler.compile" >:: test_compile;
  "Test Compiler.compile: no minizinc model filepath" >::
    test_compile_with_no_minizinc_model_filepath;
  "Test Compiler.compile: no patch (BIR)" >:: test_compile_with_no_patch;
]
