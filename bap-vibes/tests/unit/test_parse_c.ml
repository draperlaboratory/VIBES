open Bap_vibes
open OUnit2
open Core_kernel
open Bap_core_theory
open Bap_knowledge
open Knowledge.Syntax
open Knowledge.Let

let sem_str sem =
  let full_str = Format.asprintf "%a" KB.Value.pp sem in
  let sexp = Sexp.of_string full_str in
  match sexp with
  | List (List (Atom s::hd)::_) when String.is_substring s ~substring:"core:" ->
    Sexp.to_string (List hd)
  | _ -> "()"


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
          let module Eval = Core_c.Eval(T) in
          let* sem = Eval.c_patch_to_eff Helpers.dummy_target ast in
          let sem_str = sem_str sem in
          KB.return @@
          assert_equal ~cmp:String.equal ~printer:ident sem_str s2
        end


let test_var_decl _ = assert_parse_eq "int x, y, z;" "()"

let test_assign _ = assert_parse_eq "int x, y; x = y;" "(((set x y)))"

let test_seq _ = assert_parse_eq "int x, y, z; x = y; y = z;" "((((set x y)(set y z))))"

let test_ite _ =
  assert_parse_eq
    "int cond_expr; if(cond_expr){goto l1;}else{goto l2;};"
    "(((if cond_expr(goto l1)(goto l2))))"

let test_fallthrough _ =
  (* skip_if true "Still a bug in the fallthrough case"; *)
  assert_parse_eq
    "int x, y, z; if (x) { goto l; } else { x = y; } x = z; "
    "((((if x(goto l)(set x y))(set x z))))"

let test_array _ =
  assert_parse_eq
    "int a, y; y = a[7];"
    "(((set y(loadw 32 0 mem(+ a 0x7)))))"

let test_compound _ = assert_parse_eq
  "int x, y, z;
   char q;
   x = 0x7;
   x = *y;
   if(x > 0){
       goto fred;
   } else {
       goto larry;
   }"
  "(((((set x 0x7)(set x(loadw 32 0 mem y)))(if(s> x 0x0)(goto fred)(goto larry)))))"

let test_call_hex _ = assert_parse_eq
  "int temp;
   if (temp == 0)
    { (0x3ec)(); }"
  "(((if(= temp 0x0)(goto 0x3ec)())))"

let suite = [
  "Test vardecls" >:: test_var_decl;
  "Test assignment" >:: test_assign;
  "Test seq" >:: test_seq;
  "Test ite" >:: test_ite;
  "Test array" >:: test_array;
  "Test fallthrough" >:: test_fallthrough;
  "Test compound" >:: test_compound;
  "Test call hex" >:: test_call_hex;
]
