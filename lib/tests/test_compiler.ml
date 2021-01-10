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
    Patches.get_BIL H.patch 32 >>= fun bil ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_bil patch bil >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.PatchSet.singleton patch) >>= fun _ ->

    (* Now run the compiler. *)
    Compiler.compile obj >>= fun _ ->
    Data.Patched_exe.get_patches obj >>= fun patches ->
    match Data.PatchSet.to_list patches with
    | [] -> assert_failure "Result patch missing."
    | (p :: []) -> KB.return p
    | _ -> assert_failure "Multiple patches returned when one expected."
  in

  (* The complier should stash the assembly it produces in the KB. *)
  let patch_value = KB.run Data.Patch.patch computation KB.empty in
  let expected = Some H.assembly in
  H.assert_property
    ~cmp:(Option.equal (List.equal String.equal))
    ~p_res:H.print_string_list_opt ~p_expected:H.print_string_list_opt
    Data.Patch.assembly expected patch_value

(* Test that [Compiler.compile] handles no patch (BIL) in the KB. *)
let test_compile_with_no_patch (_ : test_ctxt) : unit =

  (* Run the compiler. *)
  let computation =
    (* At the start, the KB is empty. No patch (BIL) is stashed in it. *)
    H.obj () >>= fun obj ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patched_exe.set_patches obj
      (Data.PatchSet.singleton patch) >>= fun _ ->
    Compiler.compile ~solver:dummy_solver obj >>= fun _ ->
    Data.Patched_exe.get_patches obj >>= fun patches ->
    match Data.PatchSet.to_list patches with
    | [] -> assert_failure "Result patch missing."
    | (p :: []) -> KB.return p
    | _ -> assert_failure "Multiple patches returned when one expected."
  in
  let result = KB.run Data.Patch.patch computation KB.empty in
  (* The complier should produce an empty patch (no instructions, but a
     label). *)
  let expected = Some ["00000001:"] in
  H.assert_property
    ~cmp:(Option.equal (List.equal String.equal))
    ~p_res:H.print_string_list_opt ~p_expected:H.print_string_list_opt
    Data.Patch.assembly expected result

let suite = [
  "Test Compiler.compile" >:: test_compile;
  "Test Compiler.compile: no patch (BIL)" >:: test_compile_with_no_patch;
]
