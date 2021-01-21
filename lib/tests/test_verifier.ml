open !Core_kernel
open Bap_knowledge
open Knowledge.Syntax
open Bap_vibes
open OUnit2

module KB = Knowledge
module H = Helpers

(* A KB class to stash test results in. *)
module Test_data = struct
  type cls = Test_data
  let package = "vibes"
  let name = "test-data-for-verifier"
  let cls : (cls, unit) KB.cls = KB.Class.declare ~package name ()
  let result : (cls, string) KB.slot =
    KB.Class.property ~package cls
      "test-result-for-verifier" KB.Domain.string
end

(* Test that [Verifier.verify] works as expected when [UNSAT]. *)
let test_verify_unsat (_ : test_ctxt) : unit =

  (* Run the verifier. *)
  let computation =

    (* Set up the KB with the required data. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_prog obj (Some H.prog) >>= fun _ ->
    Data.Patched_exe.set_tmp_filepath obj (Some H.patched_exe) >>= fun _ ->
    Data.Verifier.set_property obj (Some H.property) >>= fun _ ->
    Data.Verifier.set_func obj (Some H.func) >>= fun _ ->

    (* Now run the verifier. Stash the result in [Test_data.result]. *)
    KB.Object.create Test_data.cls >>= fun test_data ->
    Verifier.verify obj ~loader:H.loader ~verifier:H.verify_unsat
      ~printer:H.verifier_printer >>= fun result ->
    match result with
    | Verifier.Done ->
      begin
        KB.provide Test_data.result test_data "Done" >>= fun _ ->
        KB.return test_data
      end
    | Verifier.Again _ ->
      begin
        KB.provide Test_data.result test_data "Again" >>= fun _ ->
        KB.return test_data
      end
  in
  let result = KB.run Test_data.cls computation KB.empty in

  (* It should be [Done]. *)
  let expected = "Done" in
  H.assert_property ~cmp:String.equal
    ~p_res:(Format.sprintf "%s") ~p_expected:(Format.sprintf "%s")
    Test_data.result expected result

(* Test that [Verifier.verify] works as expected when [SAT]. *)
let test_verify_sat (_ : test_ctxt) : unit =

  (* Run the verifier. *)
  let computation =

    (* Set up the KB with the required data. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_prog obj (Some H.prog) >>= fun _ ->
    Data.Patched_exe.set_tmp_filepath obj (Some H.patched_exe) >>= fun _ ->
    Data.Verifier.set_property obj (Some H.property) >>= fun _ ->
    Data.Verifier.set_func obj (Some "main") >>= fun _ ->

    (* Now run the verifier. Stash the result in [Test_data.result]. *)
    KB.Object.create Test_data.cls >>= fun test_data ->
    Verifier.verify obj ~loader:H.loader ~verifier:H.verify_sat
      ~printer:H.verifier_printer >>= fun result ->
    match result with
    | Verifier.Done ->
      begin
        KB.provide Test_data.result test_data "Done" >>= fun _ ->
        KB.return test_data
      end
    | Verifier.Again _ ->
      begin
        KB.provide Test_data.result test_data "Again" >>= fun _ ->
        KB.return test_data
      end
  in
  let result = KB.run Test_data.cls computation KB.empty in

  (* NOTE: If Z3 returns [UNSAT], then the [Verifier] should return [Again]
     to indicate that the CEGIS loop needs to try again. For now, though,
     the [Verifier] just halts with an error. *)
  let expected = Errors.Problem (Errors.Other "Halting for now.") in
  H.assert_error ~printer:(fun s -> s)
    Test_data.result expected result

(* Test that [Verifier.verify] errors without an original exe program. *)
let test_verify_with_no_original_exe_prog (_ : test_ctxt) : unit =

  (* Run the verifier. *)
  let computation =

    (* Set up the KB with nothing in it. *)
    H.obj () >>= fun obj ->

    (* Run the verifier. *)
    KB.Object.create Test_data.cls >>= fun test_data ->
    Verifier.verify obj ~loader:H.loader ~verifier:H.verify_sat
      ~printer:H.verifier_printer >>= fun _ ->
    KB.return test_data

  in
  let result = KB.run Test_data.cls computation KB.empty in

  (* The computation should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_original_exe_prog in
  H.assert_error ~printer:(Format.sprintf "%s")
    Test_data.result expected result

(* Test that [Verifier.verify] errors without an patched exe filepath. *)
let test_verify_with_no_patched_exe (_ : test_ctxt) : unit =

  (* Run the verifier. *)
  let computation =

    (* Set up the KB with no patched exe filepath. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_prog obj (Some H.prog) >>= fun _ ->

    (* Now run the verifier. *)
    KB.Object.create Test_data.cls >>= fun test_data ->
    Verifier.verify obj ~loader:H.loader ~verifier:H.verify_sat
      ~printer:H.verifier_printer >>= fun _ ->
    KB.return test_data
    
  in
  let result = KB.run Test_data.cls computation KB.empty in

  (* The computation should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_tmp_patched_exe_filepath in
  H.assert_error ~printer:(Format.sprintf "%s")
    Test_data.result expected result

(* Test that [Verifier.verify] errors without a correctness property. *)
let test_verify_with_no_property (_ : test_ctxt) : unit =

  (* Run the verifier. *)
  let computation =

    (* Setup KB with no correctness property. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_prog obj (Some H.prog) >>= fun _ ->
    Data.Patched_exe.set_tmp_filepath obj (Some H.patched_exe) >>= fun _ ->

    (* Now run the verifier. *)
    KB.Object.create Test_data.cls >>= fun test_data ->
    Verifier.verify obj ~loader:H.loader ~verifier:H.verify_sat
      ~printer:H.verifier_printer  >>= fun _ ->
    KB.return test_data

  in
  let result = KB.run Test_data.cls computation KB.empty in

  (* The computation should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_property in
  H.assert_error ~printer:(Format.sprintf "%s")
    Test_data.result expected result

(* Test that [Verifier.verify] errors without a function to verify. *)
let test_verify_with_no_func (_ : test_ctxt) : unit =

  (* Run the verifier. *)
  let computation =

    (* Setup KB with no func. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_prog obj (Some H.prog) >>= fun _ ->
    Data.Patched_exe.set_tmp_filepath obj (Some H.patched_exe) >>= fun _ ->
    Data.Verifier.set_property obj (Some H.property) >>= fun _ ->

    (* Now run the verifier. *)
    KB.Object.create Test_data.cls >>= fun test_data ->
    Verifier.verify obj ~loader:H.loader ~verifier:H.verify_sat
      ~printer:H.verifier_printer  >>= fun _ ->
    KB.return test_data

  in
  let result = KB.run Test_data.cls computation KB.empty in

  (* The computation should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_func in
  H.assert_error ~printer:(Format.sprintf "%s")
    Test_data.result expected result

let suite = [
  "Test Verifier.verify: UNSAT" >:: test_verify_unsat;
  "Test Verifier.verify: SAT" >:: test_verify_sat;
  "Test Verifier.verify: no lifted original exe" >::
  test_verify_with_no_original_exe_prog;
  "Test Verifier.verify: no patched exe filepath" >::
  test_verify_with_no_patched_exe;
  "Test Verifier.verify: no correctness property" >::
  test_verify_with_no_property;
  "Test Verifier.verify: no func" >::
  test_verify_with_no_func;
]
