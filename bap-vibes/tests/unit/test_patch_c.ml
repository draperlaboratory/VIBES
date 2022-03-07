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

open Core_kernel
open Bap_core_theory
open KB.Let
open Bap_vibes
open Bap.Std
open OUnit2

let assert_error s =
  match Parse_c.parse_c_patch s with
  | Error e ->
    assert_failure
      (Printf.sprintf "FrontC failed to parse:\n\n%s\n\nwith error %s" s e)
  | Ok ast -> try Toplevel.exec begin
      let* prog = Patch_c.translate ast ~target:(Helpers.the_target ()) in
      KB.return @@ failwithf
        "Expected error:\n\n%s\n\nis not well-formed"
        (Patch_c.to_string prog) ()
    end with Toplevel.Conflict _ -> ()

let assert_ok s =
  match Parse_c.parse_c_patch s with
  | Error e ->
    assert_failure
      (Printf.sprintf "FrontC failed to parse:\n\n%s\n\nwith error %s" s e)
  | Ok ast -> Toplevel.exec begin
      let* _prog = Patch_c.translate ast ~target:(Helpers.the_target ()) in
      KB.return ()
    end

let assert_eq s p =
  match Parse_c.parse_c_patch s with
  | Error e ->
    assert_failure
      (Printf.sprintf "FrontC failed to parse:\n\n%s\n\nwith error %s" s e)
  | Ok ast -> Toplevel.exec begin
      let* prog = Patch_c.translate ast ~target:(Helpers.the_target ()) in
      let _, stmt = prog.body in
      KB.return @@ assert_equal p stmt
        ~cmp:Patch_c.Stmt.equal
        ~printer:Patch_c.Stmt.to_string
    end

let test_void_ptr_implicit_downcast _ =
  assert_ok
    "int *x;
     void* (*malloc)(int);
     x = malloc(42);"

let test_void_ptr_implicit_upcast _ =
  assert_ok
    "void *x;
     int* (*f)(int);
     x = f(5);"

let test_bad_ptr_cast _ =
  assert_error
    "int *x;
     char* (*f)();
     x = f();"

let test_bad_arg_arity _ =
  assert_error
    "void (*f)(int, int);
     f(1, 2, 3);"

let test_bad_arg_types _ =
  assert_error
    "void (*f)(int, int);
     f(1, (int*)2);"

let test_discard _ =
  assert_ok
    "int (*maybe_effectful)(int);
     (void)maybe_effectful(5);"

let test_bad_discard _ =
  assert_error
    "int x, y;
     x = (void)(y = 5);"

let test_ptr_arith _ =
  assert_ok
    "int x, *y, z;
     z = *(y + x);"

let test_bad_ptr_arith _ =
  assert_error
    "int x, *y, z;
     z = *(y / x);"

let test_bad_lvalue_posincr _ =
  assert_error
    "int x, y;
     x++ = y;"

let test_char_assign _ =
  let p =
    let open Patch_c in
    let s = Theory.Bitv.define 8 in
    let v = Theory.Var.(forget @@ define s "x") in
    let t = INT (`r8, UNSIGNED) in
    let c = Word.of_int ~width:8 @@ Char.to_int 'a' in
    let c = Word.signed c in
    ASSIGN ((v, t), CONST_INT (c, UNSIGNED)) in
  assert_eq "char x; x = 'a';" p

let test_char_assign_ext _ =
  let p =
    let open Patch_c in
    let s = Theory.Bitv.define 32 in
    let v = Theory.Var.(forget @@ define s "x") in
    let t = INT (`r32, SIGNED) in
    let c = Word.of_int ~width:32 @@ Char.to_int 'a' in
    let c = Word.signed c in
    ASSIGN ((v, t), CONST_INT (c, SIGNED)) in
  assert_eq "int x; x = 'a';" p

let test_int_assign _ =
  let p =
    let open Patch_c in
    let s = Theory.Bitv.define 32 in
    let v = Theory.Var.(forget @@ define s "x") in
    let t = INT (`r32, SIGNED) in
    let c = Word.of_int ~width:32 255 in
    let c = Word.signed c in
    ASSIGN ((v, t), CONST_INT (c, SIGNED)) in
  assert_eq "int x; x = 255;" p

let test_char_assign_signed_ext _ =
  let p =
    let open Patch_c in
    let s = Theory.Bitv.define 32 in
    let v = Theory.Var.(forget @@ define s "x") in
    let t = INT (`r32, SIGNED) in
    let c = Word.of_int ~width:32 (-1) in
    let c = Word.signed c in
    ASSIGN ((v, t), CONST_INT (c, SIGNED)) in
  assert_eq "int x; x = (signed char)255;" p

let test_char_assign_unsigned_ext _ =
  let p =
    let open Patch_c in
    let s = Theory.Bitv.define 32 in
    let v = Theory.Var.(forget @@ define s "x") in
    let t = INT (`r32, SIGNED) in
    let c = Word.of_int ~width:32 255 in
    let c = Word.signed c in
    ASSIGN ((v, t), CONST_INT (c, SIGNED)) in
  assert_eq "int x; x = (char)255;" p

(* NOTE: this is actually a bug with FrontC. The precedence of
   the cast versus the unary minus operator is not correct.

   See example 9 in:
   https://people.eecs.berkeley.edu/~necula/cil/cil016.html
*)
let test_cast_precedence _ =
  let p =
    let open Patch_c in
    let s = Theory.Bitv.define 32 in
    let v = Theory.Var.(forget @@ define s "x") in
    let t = INT (`r32, UNSIGNED) in
    let t' = INT (`r32, SIGNED) in
    let cl = Word.of_int ~width:32 1 in
    let cr = Word.of_int ~width:32 8 in
    ASSIGN (
      (v, t),
      CAST (
        t,
        UNARY (
          MINUS,
          BINARY (
            DIV,
            CONST_INT (cl, SIGNED),
            CONST_INT (cr, SIGNED),
            t'),
          t'))) in
  assert_eq
    "unsigned long x;
     x = (unsigned long) - 1 / 8;" p

let suite = [
  "Test void pointer implicit downcast" >:: test_void_ptr_implicit_downcast;
  "Test void pointer implicit upcast" >:: test_void_ptr_implicit_upcast;
  "Test bad pointer cast" >:: test_bad_ptr_cast;
  "Test bad arg arity" >:: test_bad_arg_arity;
  "Test bad arg types" >:: test_bad_arg_types;
  "Test discard" >:: test_discard;
  "Test bad discard" >:: test_bad_discard;
  "Test pointer arithmetic" >:: test_ptr_arith;
  "Test bad pointer arithmetic" >:: test_bad_ptr_arith;
  "Test bad lvalue post-increment" >:: test_bad_lvalue_posincr;
  "Test char assign" >:: test_char_assign;
  "Test char assign ext" >:: test_char_assign_ext;
  "Test int assign" >:: test_int_assign;
  "Test char assign signed ext" >:: test_char_assign_signed_ext;
  "Test char assign unsigned ext" >:: test_char_assign_unsigned_ext;
  "Test cast precedence" >:: test_cast_precedence;
]
