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
open Bap.Std
open Bap_core_theory
open KB.Let
open Bap_wp

open Bap_vibes
open OUnit2

module Hvar = Higher_var
module H = Helpers
module Naming = Substituter.Naming

module Test_result = struct
  type cls = Test_result
  type value = (cls, unit) KB.cls KB.value
  let package = "test-result"
  let cls : (cls, unit) KB.cls = KB.Class.declare ~package "test-result" ()
  let domain : Blk.t list KB.Domain.t = KB.Domain.flat
    ~empty:[]
    ~equal:(fun x y -> List.equal Blk.equal x y)
    "blk-domain"
  let result = KB.Class.property ~package cls "test-result" domain
end

let x86_tgt = Theory.Target.get ~package:"bap" "x86_64"

(* Very lax equality to avoid tid comparison failures *)
let eq_elt e1 e2 =
  match e1, e2 with
  | `Def d1, `Def d2 ->
    let v1, e1 = Def.lhs d1, Def.rhs d1 in
    let v2, e2 = Def.lhs d2, Def.rhs d2 in
    Var.(v1 = v2) && Exp.(e1 = e2)
  | `Jmp j1, `Jmp j2 ->
    begin
      match Jmp.guard j1, Jmp.guard j2 with
      | None, None -> true
      | Some g1, Some g2 ->
        let g1 = KB.Value.get Exp.slot g1 in
        let g2 = KB.Value.get Exp.slot g2 in
        Exp.(g1 = g2)
      | _ -> false
    end
  (* We shouldn't hit any of these *)
  | `Phi _, `Phi _ -> false
  | _ -> false

let eq_blk_list b1 b2 =
  let b1 = List.concat_map b1
      ~f:(fun b -> Blk.elts b |> Seq.to_list)
  in
  let b2 = List.concat_map b2
      ~f:(fun b -> Blk.elts b |> Seq.to_list)
  in
  List.equal eq_elt b1 b2

let to_bir (code : Bil.t) =
  Bil_to_bir.bil_to_sub code |>
  Term.to_sequence blk_t |>
  Seq.to_list |> List.map ~f:(fun blk ->
      (* Any jmps that occur after an unconditional jmp should be dropped
         from the blk. *)
      let jmps =
        Term.enum jmp_t blk |> Seq.to_list |>
        List.fold_until ~init:[] ~finish:List.rev ~f:(fun acc jmp ->
            let acc = jmp :: acc in
            if Bir_helpers.is_unconditional jmp
            then Stop (List.rev acc)
            else Continue acc) in
      let defs = Seq.to_list @@ Term.enum def_t blk in
      let tid = Term.tid blk in
      Blk.create ~tid ~defs ~jmps ())

let do_subst (hvars : Hvar.t list) (code : Bil.t)
  : Test_result.cls KB.obj KB.t =
  let code = to_bir code in
  let* obj = KB.Object.create Test_result.cls in
  let entry_tid = Term.tid @@ List.hd_exn code in
  let* lower_code = Substituter.substitute
      ~tgt:x86_tgt ~hvars ~entry_tid code in
  let* () = KB.provide Test_result.result obj lower_code in
  KB.return obj

let get_result (h_vars : Hvar.t list) (code : Bil.t)
    : (Blk.t list, KB.conflict) Stdlib.result =
  let state = Toplevel.current () in
  let f = do_subst h_vars code in
  match KB.run Test_result.cls f state with
  | Error e -> Error e
  | Ok (value, _) ->
    let result = KB.Value.get Test_result.result value in
    Ok result

let str_of_blks (code : blk term list) =
  List.to_string ~f:Blk.to_string code

(* Verify that a higher var stored in a register is handled correctly. *)
let test_substitute_1 (_ : test_ctxt) : unit =
  let h_vars = Hvar.[
      create_with_registers "x"
        ~at_entry:None
        ~at_exit:(Some "RAX");
    ]
  in
  let x = Var.create "x" (Bil.Imm 64) in
  let rax = Var.create "RAX" (Bil.Imm 64) in
  let rax_reg = Naming.mark_reg rax in
  let num_3 = Word.of_int ~width:64 3 in
  let code =
    Bil.[
      x := int @@ num_3;
      if_ (var x = int num_3)
        [jmp (var x)]
        [];
    ]
  in
  let expected =
    Bil.[
      x := int @@ num_3;
      if_ (var x = int num_3)
        [rax_reg := var x; jmp (var x)]
        [];
      rax_reg := var x;
    ] |> to_bir
  in

  match get_result h_vars code with
  | Ok result ->
    let msg = Format.asprintf
        "Expected '%s' but got '%s'"
        (str_of_blks expected) (str_of_blks result)
    in
    let comparison = eq_blk_list result expected in
    assert_bool msg comparison
  | Error e ->
    let msg = Format.asprintf
      "Expected a value but got error: %a\n%!" KB.Conflict.pp e
    in
    assert_bool msg false

(* Verify that a higher var stored on the stack is handled correctly. *)
let test_substitute_2 (_ : test_ctxt) : unit =
  let h_vars = Hvar.[
      create_with_memory "x"
        ~memory:(create_frame "RBP" @@
                 Word.of_int ~width:64 0x14);
    ]
  in
  let x = Var.create "x" (Bil.Imm 64) in
  let rbp = Var.create "RBP" (Bil.Imm 64) in
  let rbp_reg = Naming.mark_reg rbp in
  let mem = Var.create "mem" (Bil.Mem (`r64, `r8)) in
  let num_3 = Word.of_int ~width:64 3 in
  let num_14 = Word.of_string "0x14:64" in
  let code =
    Bil.[
      x := int @@ num_3;
      if_ (var x = int num_3)
        [jmp (var x)]
        [];
    ]
  in
  let expected =
    Bil.[
      mem :=
        store
          ~mem:(var mem)
          ~addr:(var rbp_reg + int num_14)
          (int num_3)
          LittleEndian
          `r64;
      if_ (load
             ~mem:(var mem)
             ~addr:(var rbp_reg + int num_14)
             LittleEndian `r64
           = int num_3)
        [jmp (load
             ~mem:(var mem)
             ~addr:(var rbp_reg + int num_14)
             LittleEndian `r64)]
        []
    ] |> to_bir
  in

  match get_result h_vars code with
  | Ok result ->
    let msg = Format.asprintf
      "Expected '%s' but got '%s'" (str_of_blks expected) (str_of_blks result)
    in
    let comparison = eq_blk_list result expected in
    assert_bool msg comparison
  | Error e ->
    let msg = Format.asprintf
      "Expected a value but got error: '%a'\n%!" KB.Conflict.pp e
    in
    assert_bool msg false

(* Verify that substitution errors are raised correctly. *)
let test_substitute_error (_ : test_ctxt) : unit =
  let h_vars = Hvar.[
      create_with_registers "x"
        ~at_entry:(Some "FAKEREGISTER1")
        ~at_exit:(Some "FAKEREGISTER2");
    ]
  in
  let x = Var.create "x" (Bil.Imm 64) in
  let code = Bil.[x := var x] in

  match get_result h_vars code with
  | exception (Substituter.Subst_err _) ->
    assert_bool "Ok" true
  | Ok result ->
    let msg = Format.asprintf
      "Expected error, but got a value: '%s'" (str_of_blks result)
    in
    assert_bool msg false
  | Error e ->
    begin
      match e with
      | _ ->
        let msg = Format.asprintf
          "Expected Substituter.Subst_err exception, but got error '%a'"
          KB.Conflict.pp e
        in
        assert_bool msg false
    end

let suite = [
  "Test Substituter.substitute : case #1" >:: test_substitute_1;
  "Test Substituter.substitute : case #2" >:: test_substitute_2;
  "Test Substituter.substitute : error case" >:: test_substitute_error;
]
