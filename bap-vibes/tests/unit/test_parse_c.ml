open Bap_vibes
open OUnit2
open Core_kernel
open Bap_core_theory
open Bap_knowledge
open Knowledge.Syntax
open Knowledge.Let

let assert_parse_eq s1 s2 =
  match Parse_c.parse_c_patch s1 with
    | Error e ->
      assert_failure
        (Printf.sprintf "FrontC failed to parse %s\n with error %s" s1 e)
    | Ok ast ->
      Bap.Std.Toplevel.exec
        begin
          Theory.instance () >>=
          Theory.require >>= fun (module T) ->
          let module Eval = Parse_c.Eval(T) in
          let* sem = Eval.c_patch_to_eff Helpers.dummy_target ast in
          let sem_str = Format.asprintf "%a" KB.Value.pp sem in
          KB.return @@
          assert_equal ~cmp:String.equal ~printer:ident sem_str s2
        end


let test_var_decl _ = assert_parse_eq "int x, y, z;" "()"

let test_assign _ = assert_parse_eq "int x, y; x = y;" "((var-decls)(set x y))"

let test_ite _ =
  assert_parse_eq
    "int cond_expr; if(cond_expr){goto l1;}else{goto l2;};"
    "((var-decls) (branch cond_expr (goto l1) (goto l2)))"

let test_fallthrough _ =
  assert_parse_eq
    "goto fallthrough;"
    "((var-decls) fallthrough)"

let test_array _ =
  assert_parse_eq
    "int a, y; y = a[7];"
    "((var-decls) (set y (load (+ a 7))))"

let test_compound _ = assert_parse_eq
  "int x, y, z;
  char q;
  x = 0x7;
  x = *y;
  if(x > 0){
      goto fred;
  } else{
      goto larry;
  }"
  "
  ((var-decls x y z q)
  (set x 0x7)
  (set x (load y))
  (branch (> x 0) (goto fred) (goto larry))
  )"

let suite = [
  "Test vardecls" >:: test_var_decl;
  "Test assignment" >:: test_assign;
  (* "Test ite" >:: test_ite;
   * "Test array" >:: test_array;
   * "Test fallthrough" >:: test_fallthrough;
   * "Test compound" >:: test_compound; *)
]
