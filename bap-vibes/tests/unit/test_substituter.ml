open !Core_kernel
open Bap.Std
open Bap_core_theory
open KB.Let

open Bap_vibes
open OUnit2

module Hvar = Higher_var
module H = Helpers

module Test_result = struct
  type cls = Test_result
  type value = (cls, unit) KB.cls KB.value
  let package = "test-result"
  let cls : (cls, unit) KB.cls = KB.Class.declare ~package "test-result" ()
  let domain : Sexp.t list KB.Domain.t = KB.Domain.flat
    ~empty:[]
    ~equal:(fun x y -> List.equal Sexp.equal x y)
    "sexp-domain"
  let result = KB.Class.property ~package cls "test-result" domain
end

let do_subst (h_vars : Hvar.t list) (code : Sexp.t list)
    : Test_result.cls KB.obj KB.t =
  let* obj = KB.Object.create Test_result.cls in
  let* lower_code = Substituter.substitute h_vars code in
  let* () = KB.provide Test_result.result obj lower_code in
  KB.return obj

let get_result (h_vars : Hvar.t list) (code : Sexp.t list)
    : (Sexp.t list, KB.conflict) Stdlib.result =
  let state = Toplevel.current () in
  let f = do_subst h_vars code in
  match KB.run Test_result.cls f state with
  | Error e -> Error e
  | Ok (value, _) ->
    let result = KB.Value.get Test_result.result value in
    Ok result

let str_of = Sexp.to_string
let str_of_2 (sexps : Sexp.t list) =
  String.concat ~sep:"" (List.map sexps ~f:str_of) 

(* Verify that a higher var stored in a register is handled correctly. *)
let test_substitute_1 (_ : test_ctxt) : unit =
  let h_vars =
    [
      Hvar.create "x" (Hvar.Register "RAX") (Hvar.Register "RAX");
    ]
  in
  let code_str = 
    "(var-decls x)" ^
    "(set x 0x3)" ^
    "(branch (== x 0x3) (jmp x) fallthrough)"
  in
  let code = H.sexps_of code_str in
  let expected_str =
    "(var-decls RAX)" ^
    "(set RAX 0x3)" ^
    "(branch (== RAX 0x3) (jmp RAX) fallthrough)"
  in
  let expected = H.sexps_of expected_str in

  match get_result h_vars code with
  | Ok result ->
    let msg = Format.asprintf
      "Expected '%s' but got '%s'" (str_of_2 expected) (str_of_2 result)
    in
    let comparison = List.equal Sexp.equal result expected in
    assert_bool msg comparison
  | Error e ->
    let msg = Format.asprintf
      "Expected a value but got error: %a\n%!" KB.Conflict.pp e
    in
    assert_bool msg false

(* Verify that a higher var stored on the stack is handled correctly. *)
let test_substitute_2 (_ : test_ctxt) : unit =
  let h_vars =
    [
      Hvar.create "x"
        (Hvar.Memory ("RBP", Bitvec.of_string "0x14")) (Hvar.Register "RAX");
    ]
  in
  let code_str = 
    "(var-decls x)" ^
    "(set x 0x3)" ^
    "(branch (== x 0x3) (jmp x) fallthrough)"
  in
  let code = H.sexps_of code_str in
  let expected_str =
    "(var-decls)" ^
    "(set mem (store mem (- RBP 0x14) 0x3))" ^
    "(branch (== (load (- RBP 0x14)) 0x3) " ^
      "(jmp (load (- RBP 0x14)))" ^
      "fallthrough)"
  in
  let expected = H.sexps_of expected_str in

  match get_result h_vars code with
  | Ok result ->
    let msg = Format.asprintf
      "Expected '%s' but got '%s'" (str_of_2 expected) (str_of_2 result)
    in
    let comparison = List.equal Sexp.equal result expected in
    assert_bool msg comparison
  | Error e ->
    let msg = Format.asprintf
      "Expected a value but got error: '%a'\n%!" KB.Conflict.pp e
    in
    assert_bool msg false

(* Verify that substitution errors are raised correctly. *)
let test_substitute_error (_ : test_ctxt) : unit =
  let h_vars = [] in
  let code = [] in
  
  match get_result h_vars code with
  | Ok result ->
    let msg = Format.asprintf
      "Expected error, but got a value: '%s'" (str_of_2 result)
    in
    assert_bool msg false
  | Error e ->
    begin
      match e with
      | Kb_error.Problem (Kb_error.Higher_vars_not_substituted _) ->
        assert_bool "Ok" true
      | _ ->
        let msg = Format.asprintf
          "Expected Kb_error.Higher_vars_not_substituted but got '%a'"
          KB.Conflict.pp e
        in
        assert_bool msg false
    end

let suite = [
  "Test Substituter.substitute : case #1" >:: test_substitute_1;
  "Test Substituter.substitute : case #2" >:: test_substitute_2;
  "Test Substituter.substitute : error case" >:: test_substitute_error;
]
