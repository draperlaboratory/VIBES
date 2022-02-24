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
]
