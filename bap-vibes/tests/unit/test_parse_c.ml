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

let fake_var = Var.create "virt" Unk

let fix_bil_names sem =
  let mapper = object(self)
    inherit Stmt.mapper
    method! map_var (v : var) : exp =
      let v = if Var.is_virtual v then fake_var else v in
      Var (Substituter.unmark_reg v |> Option.value ~default:v)
    method! map_move (v : var) (e : exp) : stmt list =
      let v = if Var.is_virtual v then fake_var else v in
      let v = Substituter.unmark_reg v |> Option.value ~default:v in
      let e = self#map_exp e in
      Bil.[v := e]
  end in
  Stmt.map mapper sem

let eff_to_str sem =
  Format.asprintf "%a" Bil.pp sem |>
  String.filter ~f:(fun c -> not Char.(c = '\"'))

let virtual_regex = Str.regexp "#[0-9]+"

let compare_sem sem str =
  let strip s = String.filter s ~f:(Fn.non Char.is_whitespace) in
  String.equal (strip sem) (strip str)

let assert_parse_eq ?(hvars = []) s1 s2 =
  match Parse_c.parse_c_patch s1 with
  | Error e ->
    assert_failure
      (Printf.sprintf "FrontC failed to parse %s\n with error %s" s1 e)
  | Ok ast ->
    Bap.Std.Toplevel.exec
      begin
        let* theory = Theory.instance () in
        let* (module T) = Theory.require theory in
        let module Eval = Core_c.Eval(T) in
        let* sem = Eval.c_patch_to_eff hvars (Helpers.the_target ()) ast in
        let sem = fix_bil_names @@ Insn.bil sem in
        let sem_str = eff_to_str sem in
        KB.return @@ assert_equal ~cmp:compare_sem ~printer:ident s2 sem_str
      end

let test_var_decl _ =
  assert_parse_eq
    "int x, y, z;"
    "{ }"

let test_assign _ =
  assert_parse_eq
    "int x, y; x = y;"
    "{ x := y }"

let test_seq _ =
  assert_parse_eq
    "int x, y, z; x = y; y = z;"
    "{ x := y y := z }"

let test_ite _ =
  assert_parse_eq
    "int cond_expr; if(cond_expr){goto l1;}else{goto l2;};"
    "{
       if (cond_expr) {
         call(l1)
       } else {
         call(l2)
       }
     }"

let test_fallthrough _ =
  assert_parse_eq
    "int x, y, z; if (x) { goto l; } x = z; "
    "{
       if (x) { call(l) }
       x := z
     }"

let test_array _ =
  assert_parse_eq
    "int* a, y; y = a[7];"
    "{ y := mem[a + 0x1C, el]:u32 }"

let test_array_multi_ptr _ =
  assert_parse_eq
    "int** a, y; y = a[1][2];"
    "{ y := mem[mem[a + 4, el]:u32 + 8, el]:u32 }"

let test_compound _ =
  assert_parse_eq
    "int x, *y, z;
     char q;
     x = 0x7;
     x = *y;
     if(x > 0){
       goto fred;
     } else {
       goto larry;
     }"
    "{
       x := 7
       x := mem[y, el]:u32
       if (0 <$ x) {
         call(fred)
       } else {
         call(larry)
       }
     }"

let test_call_hex _ =
  assert_parse_eq
    "int temp;
     if (temp == 0) { ((void (*)())0x3ec)(); }"
    "{
       if (temp = 0) {
         jmp 0x3EC
       }
     }"

let test_call_args_1 _ =
  assert_parse_eq
    "int a, b, c;
     void (*f)(int, int, int);
     f(a, b, c);"
    "{
       R0 := a
       R1 := b
       R2 := c
       call(f)
     }"

let test_call_args_2 _ =
  assert_parse_eq
    "int a, c;
     void (*f)(int, int, int);
     f(a, 0x1234, c);"
    "{
       R0 := a
       R1 := 0x1234
       R2 := c
       call(f)
     }"

let test_call_args_eff _ =
  assert_parse_eq
    "int a, b, c;
     void (*f)(int, int, int);
     f(a, b++, ++c);"
    "{
       virt := b
       b := b + 1
       c := c + 1
       R0 := a
       R1 := virt
       R2 := c
       call(f)
     }"

let test_call_args_ret _ =
  assert_parse_eq
    "int a, b, c, d;
     int (*f)(int, int, int);
     d = f(a, b, c);"
    "{
       R0 := a
       R1 := b
       R2 := c
       call(f)
       d := R0
     }"

let test_call_args_ret_store _ =
  assert_parse_eq
    "int a, b, c;
     int *d;
     int (*f)(int, int, int);
     d = (int*)(a + 0x1337);
     *d = f(a, b, c);"
    "{
       d := a + 0x1337
       R0 := a
       R1 := b
       R2 := c
       call(f)
       virt := R0
       mem := mem with [d, el]:u32 <- virt
     }"

let test_call_args_addrof _ =
  let hvars = Higher_var.[
      create_with_storage "c"
        ~at_exit:None
        ~at_entry:(stored_in_memory
                     (create_frame "SP" @@
                      Bap.Std.Word.of_int ~width:32 8));
    ] in
  assert_parse_eq ~hvars
    "int a, b, *c, d;
     void (*f)(int, int, int**);
     f(a, b, &c);
     d = *c;"
    "{
       R0 := a
       R1 := b
       R2 := SP + 8
       call(f)
       d := mem[c, el]:u32
     }"

let test_load_short _ =
  assert_parse_eq
    "int x, y;
     x = * (short *) y;"
    "{ x := extend:32[mem[y, el]:u16] }"

let test_load_ushort _ =
  assert_parse_eq
    "int x, y;
     x = * (unsigned short *) y;"
    "{ x := pad:32[mem[y, el]:u16] }"

let test_ternary_assign _ =
  assert_parse_eq
    "int (*f)();
     int x, c;
     x = c ? f() : 5;"
    "{
      if (c) {
        call(f)
        x := R0
      }
      else {
        x := 5
      }
     }"

let test_ternary_posincr _ =
  assert_parse_eq
    "int x, y, z, c;
     z = (c ? x : y)++;"
    "{
      if (c) {
        virt := x
        x := x + 1
      }
      else {
        virt := y
        y := y + 1
      }
      z := virt
     }"

let test_ternary_eff _ =
  assert_parse_eq
    "void (*f)();
     int c, x, y;
     (c ? f() : (void)(x = 5));
     y = x;"
    "{
       if (c) {
         call(f)
       }
       else {
         x := 5
       }
       y := x
     }"

let test_posincr _ =
  assert_parse_eq
    "int x; x++;"
    "{ x := x + 1 }"

let test_posincr_assign _ =
  assert_parse_eq
    "int x, y; y = x++;"
    "{
       virt := x
       x := x + 1
       y := virt
     }"

let test_preincr _ =
  assert_parse_eq
    "int x; ++x;"
    "{ x := x + 1 }"

let test_preincr_assign _ =
  assert_parse_eq
    "int x, y; y = ++x;"
    "{
       x := x + 1
       y := x
     }"

let test_and_short_circ _ =
  assert_parse_eq
    "int x, y;
     int (*f)();
     if (x && f()) {
       y = 5;
     }"
    "{
       virt := x
       if (virt) {
         call(f)
         virt := R0
         virt := virt
       }
       if (virt) {
         y := 5
       }
     }"

let test_or_short_circ _ =
  assert_parse_eq
    "int x, y;
     int (*f)();
     if (x || f()) {
       y = 5;
     }"
    "{
       virt := x
       if (virt) {
       }
       else {
         call(f)
         virt := R0
         virt := virt
       }
       if (virt) {
         y := 5
       }
     }"

let test_add_assign _ =
  assert_parse_eq
    "int x, y;
     (x += y) += (y += x);"
    "{
       y := y + x
       x := x + y
       x := x + y
     }"

let test_ternary_compound _ =
  assert_parse_eq
    "int c, x, y, z;
     z = (c ? x : y) += 5;"
    "{
       if (c) {
         x := x + 5
         virt := x
       }
       else {
         y := y + 5
         virt := y
       }
       z := virt
     }"

let test_comma _ =
  assert_parse_eq
    "int x, y, z;
     z = (x = 5, y * x);"
    "{
       x := 5
       z := y * x
     }"

let test_comma_ambig _ =
  assert_parse_eq
    "int x, y, z;
     z = x = 5, y * x;"
    "{
       x := 5
       z := x
     }"

let test_ternary_deref _ =
  assert_parse_eq
    "int c, *x, *y;
     *(c ? x : y) = 5;"
    "{
       if (c) {
         virt := x
       }
       else {
         virt := y
       }
       mem := mem with [virt, el]:u32 <- 5
     }"

let suite = [
  "Test vardecls" >:: test_var_decl;
  "Test assignment" >:: test_assign;
  "Test seq" >:: test_seq;
  "Test ite" >:: test_ite;
  "Test array" >:: test_array;
  "Test array multi ptr" >:: test_array_multi_ptr;
  "Test fallthrough" >:: test_fallthrough;
  "Test compound" >:: test_compound;
  "Test call hex" >:: test_call_hex;
  "Test call args 1" >:: test_call_args_1;
  "Test call args 2" >:: test_call_args_2;
  "Test call args eff" >:: test_call_args_eff;
  "Test call args ret" >:: test_call_args_ret;
  "Test call args ret store" >:: test_call_args_ret_store;
  "Test call args addrof" >:: test_call_args_addrof;
  "Test load short" >:: test_load_short;
  "Test load unsigned short" >:: test_load_ushort;
  "Test ternary assign" >:: test_ternary_assign;
  "Test ternary post increment" >:: test_ternary_posincr;
  "Test ternary effect" >:: test_ternary_eff;
  "Test post increment" >:: test_posincr;
  "Test post increment assign" >:: test_posincr_assign;
  "Test pre increment" >:: test_preincr;
  "Test pre increment assign" >:: test_preincr_assign;
  "Test AND short-circuit" >:: test_and_short_circ;
  "Test OR short-circuit" >:: test_or_short_circ;
  "Test ADD_ASSIGN" >:: test_add_assign;
  "Test ternary compound" >:: test_ternary_compound;
  "Test comma" >:: test_comma;
  "Test comma ambiguous" >:: test_comma_ambig;
  "Test ternary deref" >:: test_ternary_deref;
]
