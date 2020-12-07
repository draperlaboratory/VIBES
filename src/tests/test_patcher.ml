open !Core_kernel
open Bap_knowledge
open Knowledge.Syntax

module KB = Knowledge
module H = Helpers

open OUnit2

(* A dummy patcher, that returns a fixed filename. *)
let patcher _ _ _ = KB.return H.patched_exe

(* Test that [Patcher.patch] works as expected. *)
let test_patch (ctxt : test_ctxt) : unit =

  (* Run the patcher. *)
  let computation =

    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->
    Data.Patched_exe.set_patch_point obj (Some H.patch_point) >>= fun _ ->
    Data.Patch.set_assembly obj (Some H.assembly) >>= fun _ ->

    (* Now run the patcher. *)
    Patcher.patch obj ~patcher:patcher >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should stash the patched exe's filepath in the KB. *)
  let expected = Some H.patched_exe in
  H.assert_property ~cmp:(Option.equal String.equal) ~printer:H.print_opt 
    Data.Patched_exe.tmp_filepath expected result

(* Test that [Patcher.patch] errors if there's no filepath to
   the original executable in the KB. *)
let test_patch_with_no_original_exe (ctxt : test_ctxt) : unit =

  (* Run the patcher. *)
  let computation =
    (* The KB starts with no filepath for the original_exe stashed in it. *)
    H.obj () >>= fun obj ->
    Patcher.patch obj >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_original_exe_filepath in
  H.assert_error ~printer:H.print_opt
    Data.Patched_exe.tmp_filepath expected result

(* Test that [Patcher.patch] errors if there's no patch point in the KB. *)
let test_patch_with_no_patch_point (ctxt : test_ctxt) : unit =

  (* Run the patcher. *)
  let computation =

    (* The KB starts with only a filepath for the original exe,
       and no patch_point stashed in it. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->

    (* Now run the patcher. *)
    Patcher.patch obj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_patch_point in
  H.assert_error ~printer:H.print_opt
    Data.Patched_exe.tmp_filepath expected result

(* Test that [Patcher.patch] errors if there's no assembly in the KB. *)
let test_patch_with_no_assembly (ctxt : test_ctxt) : unit =

  (* Run the patcher. *)
  let computation =

    (* The KB starts with no assembly stashed in it. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->
    Data.Patched_exe.set_patch_point obj (Some H.patch_point) >>= fun _ ->

    (* Now run the patcher. *)
    Patcher.patch obj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The patcher should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_assembly in
  H.assert_error ~printer:H.print_opt
    Data.Patched_exe.tmp_filepath expected result

let suite = [
  "Test Patcher.patch" >:: test_patch;
  "Test Patcher.patch: no original exe" >:: test_patch_with_no_original_exe;
  "Test Patcher.patch: no patch point" >:: test_patch_with_no_patch_point;
  "Test Patcher.patch: no assembly" >:: test_patch_with_no_assembly;
]
