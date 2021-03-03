open !Core_kernel
open Bap.Std
open Bap_knowledge
open Knowledge.Syntax
open Bap_vibes
open OUnit2

module KB = Knowledge
module H = Helpers


(* Test that [Exe_ingester.ingest] stashes the address size in the KB. *)
let test_ingest_addr_size (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =

    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->

    (* Now run the ingester (use the dummy [H.loader]). *)
    Exe_ingester.ingest obj ~loader:H.loader >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should stash the address size in the KB. *)
  let expected = Some 32 in
  H.assert_property
    ~p_res:H.print_int_opt ~p_expected:H.print_int_opt ~cmp:(Option.equal (=))
    Data.Original_exe.addr_size expected result

(* Test that [Exe_ingester.ingest] stashes the lifted program in the KB. *)
let test_ingest_prog (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =

    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Data.Original_exe.set_filepath obj (Some H.original_exe) >>= fun _ ->

    (* Now run the ingester (use the dummy [H.loader]). *)
    Exe_ingester.ingest obj ~loader:H.loader >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should stash the lifted program in the KB. *)
  let expected = Some H.prog in
  H.assert_property ~p_res:H.print_prog_opt ~p_expected:H.print_prog_opt
    ~cmp:(Option.equal Program.equal)
    Data.Original_exe.prog expected result

(* Test that [Exe_ingester.ingest] errors with no filepath in the KB. *)
let test_ingest_with_no_exe_filepath (_ : test_ctxt) : unit =

  (* Run the ingester. *)
  let computation =
    (* Don't set anything up in the KB, so the KB is empty. *)
    H.obj () >>= fun obj ->
    Exe_ingester.ingest obj ~loader:H.loader >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in

  (* The ingester should diverge with the appropriate error. *)
  let expected = Errors.Problem Errors.Missing_original_exe_filepath in
  H.assert_error ~printer:H.print_int_opt
    Data.Original_exe.addr_size expected result

let suite = [
  "Test Exe_ingester.ingest: stashes address size" >:: test_ingest_addr_size;
  "Test Exe_ingester.ingest: stashes lifted program" >:: test_ingest_prog;
  "Test Exe_ingester.ingest: no original exe filepath" >::
  test_ingest_with_no_exe_filepath;
]
