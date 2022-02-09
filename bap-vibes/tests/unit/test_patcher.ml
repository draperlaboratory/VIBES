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
open Bap_core_theory
open Knowledge.Syntax
open Bap_vibes
open OUnit2

module KB = Knowledge
module H = Helpers


let dummy_patch n m : Patcher.patch = {
  assembly = [
    Printf.sprintf ".rept %Ld" n;
    "nop";
    ".endr"
  ];
  orig_loc = 0L;
  orig_size = m
}

let dummy_compute_region ~loc:_ _ =
  Or_error.return Patcher.{ region_addr = 0L; region_offset = 0L }

let test_patch_placer_exact_fit _ =
  let tgt = H.the_target () in
  let lang = Arm_target.llvm_a32 in
  let patch = dummy_patch 2L 8L in
  let patch_sites = [] in
  let placed_patch = Patcher.place_patches tgt lang [patch] patch_sites |> List.hd_exn in
  assert_equal ~printer:Int64.to_string placed_patch.patch_loc 0L;
  assert_equal placed_patch.jmp None

let test_patch_placer_loose_fit _ =
  let tgt = H.the_target () in
  let lang = Arm_target.llvm_a32 in
  let patch = dummy_patch 1L 8L in
  let patch_sites : Patcher.patch_site list = [{
      location = 100L;
      size = 128L
    }] in
  let placed_patch = Patcher.place_patches tgt lang [patch] patch_sites |> List.hd_exn in
  assert_equal ~printer:Int64.to_string placed_patch.patch_loc 0L;
  assert_equal ~printer:Int64.to_string 8L (Option.value_exn placed_patch.jmp)

let test_patch_placer_no_fit _ =
  let tgt = H.the_target () in
  let lang = Arm_target.llvm_a32 in
  let patch = dummy_patch 8L 4L in
  let patch_sites : Patcher.patch_site list = [{
      location = 100L;
      size = 128L
    }] in
  match Patcher.place_patches tgt lang [patch] patch_sites with
  | [orig_jmp; placed_patch] ->
    assert_equal ~printer:Int64.to_string placed_patch.patch_loc 100L;
    assert_equal ~printer:Int64.to_string 4L (Option.value_exn placed_patch.jmp);
    assert_equal 0L orig_jmp.patch_loc;
    assert_equal 100L (Option.value_exn orig_jmp.jmp)
  | _ -> assert_failure "List is wrong size"

(* A dummy patcher, that returns a fixed filename. *)
let patcher _ ~filename:_ _ = H.patched_exe

(* Test that [Patcher.patch] works as expected. *)
let test_patch (_ : test_ctxt) : unit =


  (* Run the patcher. *)
  let computation =

    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_patch_point patch (Some H.patch_point) >>= fun _ ->
    Data.Patch.set_patch_size patch (Some 1024) >>= fun _ ->
    Data.Patch.set_assembly patch (Some H.assembly) >>= fun _ ->
    Theory.Label.for_addr H.patch_point >>= fun patch_tid ->
    H.unit >>= fun unit ->
    KB.provide Theory.Label.unit patch_tid (Some unit) >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun _ ->

    (* Now run the patcher. *)
    Patcher.patch obj
      ~patcher:patcher
      ~compute_region:dummy_compute_region >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should stash the patched exe's filepath in the KB. *)
  let expected = Some H.patched_exe in
  H.assert_property ~cmp:(Option.equal String.equal)
    ~p_res:H.print_opt ~p_expected:H.print_opt
    Data.Patched_exe.tmp_filepath expected result

(* Test that [Patcher.patch] errors if there's no filepath to
   the original executable in the KB. *)
let test_patch_with_no_original_exe (_ : test_ctxt) : unit =

  (* Run the patcher. *)
  let computation =
    (* The KB starts with no filepath for the original_exe stashed in it. *)
    H.obj () >>= fun obj ->
    Patcher.patch obj
      ~compute_region:dummy_compute_region >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should diverge with the appropriate error. *)
  let expected = Kb_error.Problem Kb_error.Missing_original_exe_filepath in
  H.assert_error ~printer:H.print_opt
    Data.Patched_exe.tmp_filepath expected result

(* Test that [Patcher.patch] errors if there's no patch point in the KB. *)
let test_patch_with_no_patch_point (_ : test_ctxt) : unit =

  (* Run the patcher. *)
  let computation =

    (* The KB starts with only a filepath for the original exe,
       and no patch_point stashed in it. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_patch_size patch (Some 1024) >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun _ ->
    (* Now run the patcher. *)
    Patcher.patch obj ~compute_region:dummy_compute_region >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should diverge with the appropriate error. *)
  let expected = Kb_error.Problem Kb_error.Missing_patch_point in
  H.assert_error ~printer:H.print_opt
    Data.Patched_exe.tmp_filepath expected result

(* Test that [Patcher.patch] errors if there's no assembly in the KB. *)
let test_patch_with_no_assembly (_ : test_ctxt) : unit =

  (* Run the patcher. *)
  let computation =

    (* The KB starts with no assembly stashed in it. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_patch_point patch (Some H.patch_point) >>= fun _ ->
    Data.Patch.set_patch_size patch (Some 1024) >>= fun _ ->
    Theory.Label.for_addr H.patch_point >>= fun patch_tid ->
    H.unit >>= fun unit ->
    KB.provide Theory.Label.unit patch_tid (Some unit) >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun _ ->

    (* Now run the patcher. *)
    Patcher.patch obj ~compute_region:dummy_compute_region >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should diverge with the appropriate error. *)
  let expected = Kb_error.Problem Kb_error.Missing_assembly in
  H.assert_error ~printer:H.print_opt
    Data.Patched_exe.tmp_filepath expected result

let suite = [
  "Test Patcher.patch" >:: test_patch;
  "Test Patcher.patch: no original exe" >:: test_patch_with_no_original_exe;
  "Test Patcher.patch: no patch point" >:: test_patch_with_no_patch_point;
  "Test Patcher.patch: no assembly" >:: test_patch_with_no_assembly;
  "Test Patcher.place_patch: exact fit" >:: test_patch_placer_exact_fit;
  "Test Patcher.place_patch: loose fit" >:: test_patch_placer_loose_fit;
  "Test Patcher.place_patch: no fit" >:: test_patch_placer_no_fit
]
