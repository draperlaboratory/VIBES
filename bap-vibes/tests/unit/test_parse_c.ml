open Bap_vibes
open OUnit2
open Core_kernel

let assert_parse_eq s1 s2 = 
    match Parse_c.c_patch_to_sexp_string s1 with
    | Error e -> assert_failure (Printf.sprintf "FrontC failed to parse %s\n with error %s" s1 e)
    | Ok s1 ->
        assert_equal ~cmp:Sexp.equal
        ~printer:Sexp.to_string
        (Sexp.of_string s2)
        (Sexp.of_string ("("^s1^")"))


let test_var_decl _ = assert_parse_eq "int x, y, z;" "((var-decls x y z))"
let test_assign _ = assert_parse_eq "x = y;" "((var-decls)(set x y))"
let test_ite _ = assert_parse_eq "if(cond_expr){goto l1;}else{goto l2;};"
        "((var-decls) (branch cond_expr (goto l1) (goto l2)))"
let test_fallthrough _ = assert_parse_eq "goto fallthrough;"
  "((var-decls) fallthrough)"
let test_array _ = assert_parse_eq "y = a[7];"
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
  "Test ite" >:: test_ite;
  "Test array" >:: test_array;
  "Test fallthrough" >:: test_fallthrough;
  "Test compound" >:: test_compound;
]