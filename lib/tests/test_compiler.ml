open !Core_kernel
open Bap_knowledge
open Knowledge.Syntax
open Bap_vibes
open OUnit2

module KB = Knowledge
module H = Helpers

let dummy_solver vir = KB.return vir

(* Test that [Compiler.compile] works as expected. *)
let test_compile (_ : test_ctxt) : unit =

  (* Run the compiler. *)
  let computation =

    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Patches.get_bir H.patch 32 >>= fun bir ->
    Data.Patch.set_bir obj bir >>= fun _ ->

    (* Now run the compiler. *)
    Compiler.compile obj >>= fun _ ->
    KB.return obj

  in
  let result = KB.run Data.cls computation KB.empty in

  (* The complier should stash the assembly it produces in the KB. *)
  let expected = Some H.assembly in
  H.assert_property
    ~cmp:(Option.equal (List.equal String.equal))
    ~p_res:H.print_string_list_opt ~p_expected:H.print_string_list_opt
    Data.Patch.assembly expected result

(* Test that [Compiler.compile] handles no patch (BIR) in the KB. *)
let test_compile_with_no_patch (_ : test_ctxt) : unit =

  (* Run the compiler. *)
  let computation =
    (* At the start, the KB is empty. No patch (BIR) is stashed in it. *)
    H.obj () >>= fun obj ->
    Compiler.compile ~solver:dummy_solver obj >>= fun _ ->
    KB.return obj
  in
  let result = KB.run Data.cls computation KB.empty in
  let expected = Errors.Problem (Errors.Missing_semantics "arm_eff not found in:()") in

  H.assert_error
    ~printer:H.print_string_list_opt
    Data.Patch.assembly
    expected
    result

let suite = [
  "Test Compiler.compile" >:: test_compile;
  "Test Compiler.compile: no patch (BIR)" >:: test_compile_with_no_patch;
]
