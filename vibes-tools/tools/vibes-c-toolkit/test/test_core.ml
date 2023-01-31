open Core
open Bap.Std
open Bap_core_theory
open Vibes_c_toolkit
open OUnit2

open KB.Syntax

module Higher_var = Vibes_higher_vars.Higher_var

let eff_to_str (bil : bil) : string =
  Format.asprintf "%a" Bil.pp bil |>  
  String.filter ~f:(fun c -> not Char.(c = '\"'))

let strip : string -> string =
  String.filter ~f:(Fn.non Char.is_whitespace)

let compare_sem (sem : string) (str : string) : bool =
  String.equal (strip sem) (strip str)

let assert_parse_eq
    ?(hvars : Higher_var.t list = [])
    (c : string)
    (bil : string) : unit =
  match Parse_c.parse c with
  | Error e ->
    assert_failure @@
    Format.asprintf "FrontC failed to parse:\n\n%s\n\nwith error %a"
      c KB.Conflict.pp e
  | Ok ast ->
    let state = Toplevel.current () in
    Toplevel.reset ();
    let result = Toplevel.var "core-c" in
    Toplevel.put result begin
      let* theory = Theory.instance () in
      let* (module Core) = Theory.require theory in
      let module C_compiler = Core_c.Make(Core) in
      let target = Theory.Target.of_string "bap:armv7+le" in
      C_compiler.compile hvars target ast
    end;
    let sem = Toplevel.get result in
    let s = eff_to_str @@ Insn.bil sem in
    Toplevel.set state;
    assert_equal bil s ~cmp:compare_sem ~printer:Fn.id

let test_var_decl (_ : test_ctxt) =
  assert_parse_eq
    "int x, y, z;"
    "{ }"

let test_assign (_ : test_ctxt) =
  assert_parse_eq
    "int x, y; x = y;"
    "{ x := y }"

let test_seq (_ : test_ctxt) =
  assert_parse_eq
    "int x, y, z; x = y; y = z;"
    "{ x := y y := z }"

let test_ite (_ : test_ctxt) =
  assert_parse_eq
    "int cond_expr;
     if(cond_expr) {
     l2:
       goto l1;
     } else {
     l1:
       goto l2;
     }"
    "{
       if (cond_expr <> 0) {
         label(%00000003)
         call(l1)
       } else {
         label(%00000004)
         call(l2)
       }
     }"

let test_fallthrough (_ : test_ctxt) =
  assert_parse_eq
    "int x, y, z; if (x) { goto l; } x = z; l: (void)x; "
    "{
       if (x <> 0) {
         call(l)
       }
       x := z
       label(%00000003)
     }"

let test_array (_ : test_ctxt) =
  assert_parse_eq
    "int* a, y; y = a[7];"
    "{ y := mem[a + 0x1C, el]:u32 }"

let test_array_multi_ptr (_ : test_ctxt) =
  assert_parse_eq
    "int** a, y; y = a[1][2];"
    "{ y := mem[mem[a + 4, el]:u32 + 8, el]:u32 }"

let test_compound (_ : test_ctxt) =
  assert_parse_eq
    "int x, *y, z;
     char q;
     larry:
     x = 0x7;
     fred:
     x = *y;
     if(x > 0){
       goto fred;
     } else {
       goto larry;
     }"
    "{
       label(%00000002)
       x := 7
       label(%00000003)
       x := mem[y, el]:u32
       if (0 <$ x) {
         call(fred)
       } else {
         call(larry)
       }
     }"

let test_call_hex (_ : test_ctxt) =
  assert_parse_eq
    "int temp;
     if (temp == 0) { ((void (*)())0x3ec)(); }"
    "{
       if (temp = 0) {
         jmp 0x3EC
       }
     }"

let test_call_args_1 (_ : test_ctxt) =
  assert_parse_eq
    "int a, b, c;
     void (*f)(int, int, int);
     f(a, b, c);"
    "{
       goto(%00000006)
       label(%00000006)
       reg:R0 := a
       reg:R1 := b
       reg:R2 := c
       call(f)
     }"

let test_call_args_2 (_ : test_ctxt) =
  assert_parse_eq
    "int a, c;
     void (*f)(int, int, int);
     f(a, 0x1234, c);"
    "{
       goto(%00000005)
       label(%00000005)
       reg:R0 := a
       reg:R1 := 0x1234
       reg:R2 := c
       call(f)
     }"

let test_call_args_eff (_ : test_ctxt) =
  assert_parse_eq
    "int a, b, c;
     void (*f)(int, int, int);
     f(a, b++, ++c);"
    "{
       #0 := b
       b := b + 1
       c := c + 1
       goto(%00000009)
       label(%00000009)
       reg:R0 := a
       reg:R1 := #0
       reg:R2 := c
       call(f)
     }"

let test_call_args_ret (_ : test_ctxt) =
  assert_parse_eq
    "int a, b, c, d;
     int (*f)(int, int, int);
     d = f(a, b, c);"
    "{
       goto(%00000006)
       label(%00000006)
       reg:R0 := a
       reg:R1 := b
       reg:R2 := c
       call(f)
       d := reg:R0
     }"

let test_call_args_ret_store (_ : test_ctxt) =
  assert_parse_eq
    "int a, b, c;
     int *d;
     int (*f)(int, int, int);
     d = (int*)(a + 0x1337);
     *d = f(a, b, c);"
    "{
       d := a + 0x1337
       goto(%00000007)
       label(%00000007)
       reg:R0 := a
       reg:R1 := b
       reg:R2 := c
       call(f)
       #0 := reg:R0
       mem := mem with [d, el]:u32 <- #0
     }"

let test_call_args_addrof (_ : test_ctxt) =
  let hvars = Higher_var.[
      {
        name = "c";
        value = Memory (Frame ("SP", Word.of_int ~width:32 8));
      };
    ] in
  assert_parse_eq ~hvars
    "int a, b, *c, d;
     void (*f)(int, int, int**);
     f(a, b, &c);
     d = *c;"
    "{
       goto(%00000006)
       label(%00000006)
       reg:R0 := a
       reg:R1 := b
       reg:R2 := reg:SP + 8
       call(f)
       d := mem[c, el]:u32
     }"

let test_load_short (_ : test_ctxt) =
  assert_parse_eq
    "int x, y;
     x = * (short *) y;"
    "{ x := extend:32[mem[y, el]:u16] }"

let test_load_ushort (_ : test_ctxt) =
  assert_parse_eq
    "int x, y;
     x = * (unsigned short *) y;"
    "{ x := pad:32[mem[y, el]:u16] }"

let test_ternary_assign (_ : test_ctxt) =
  assert_parse_eq
    "int (*f)();
     int x, c;
     x = c ? f() : 5;"
    "{
      if (c <> 0) {
        call(f)
        x := reg:R0
      }
      else {
        x := 5
      }
     }"

let test_ternary_posincr (_ : test_ctxt) =
  assert_parse_eq
    "int x, y, z, c;
     z = (c ? x : y)++;"
    "{
      if (c <> 0) {
        #0 := x
        x := x + 1
      }
      else {
        #0 := y
        y := y + 1
      }
      z := #0
     }"

let test_ternary_eff (_ : test_ctxt) =
  assert_parse_eq
    "void (*f)();
     int c, x, y;
     (c ? f() : (void)(x = 5));
     y = x;"
    "{
       if (c <> 0) {
         call(f)
       }
       else {
         x := 5
       }
       y := x
     }"

let test_posincr (_ : test_ctxt) =
  assert_parse_eq
    "int x; x++;"
    "{ x := x + 1 }"

let test_posincr_assign (_ : test_ctxt) =
  assert_parse_eq
    "int x, y; y = x++;"
    "{
       #0 := x
       x := x + 1
       y := #0
     }"

let test_preincr (_ : test_ctxt) =
  assert_parse_eq
    "int x; ++x;"
    "{ x := x + 1 }"

let test_preincr_assign (_ : test_ctxt) =
  assert_parse_eq
    "int x, y; y = ++x;"
    "{
       x := x + 1
       y := x
     }"

let test_and_short_circ (_ : test_ctxt) =
  assert_parse_eq
    "int x, y;
     int (*f)();
     if (x && f()) {
       y = 5;
     }"
    "{
       #1 := x
       if (#1 <> 0) {
         call(f)
         #0 := reg:R0
         #1 := #0
       }
       if (#1 <> 0) {
         y := 5
       }
     }"

let test_or_short_circ (_ : test_ctxt) =
  assert_parse_eq
    "int x, y;
     int (*f)();
     if (x || f()) {
       y = 5;
     }"
    "{
       #1 := x
       if (#1 <> 0) {
       }
       else {
         call(f)
         #0 := reg:R0
         #1 := #0
       }
       if (#1 <> 0) {
         y := 5
       }
     }"

let test_add_assign (_ : test_ctxt) =
  assert_parse_eq
    "int x, y;
     (x += y) += (y += x);"
    "{
       y := y + x
       x := x + y
       x := x + y
     }"

let test_ternary_compound (_ : test_ctxt) =
  assert_parse_eq
    "int c, x, y, z;
     z = (c ? x : y) += 5;"
    "{
       if (c <> 0) {
         x := x + 5
         #0 := x
       }
       else {
         y := y + 5
         #0 := y
       }
       z := #0
     }"

let test_comma (_ : test_ctxt) =
  assert_parse_eq
    "int x, y, z;
     z = (x = 5, y * x);"
    "{
       x := 5
       z := y * x
     }"

let test_comma_ambig (_ : test_ctxt) =
  assert_parse_eq
    "int x, y, z;
     z = x = 5, y * x;"
    "{
       x := 5
       z := x
     }"

let test_ternary_deref (_ : test_ctxt) =
  assert_parse_eq
    "int c, *x, *y;
     *(c ? x : y) = 5;"
    "{
       if (c <> 0) {
         #0 := x
       }
       else {
         #0 := y
       }
       mem := mem with [#0, el]:u32 <- 5
     }"

let test_char_deref_posincr (_ : test_ctxt) =
  assert_parse_eq
    "char *a, *b;
     *a++ = *b++;"
    "{
       #0 := mem[b]
       b := b + 1
       mem := mem with [a] <- #0
       a := a + 1
     }"

let test_bool_not (_ : test_ctxt) =
  assert_parse_eq
    "int x, y;
     if (!x) {
       x = y;
     }"
    "{
       if (x = 0) {
         x := y
       }
     }"

let test_mixed_sorts (_ : test_ctxt) =
  assert_parse_eq
    "int x, y;
     x += (y == 0);"
    "{
       x := x + pad:32[y = 0]
     }"

let suite : test = "Test Core C" >::: [
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
  "Test char deref post increment" >:: test_char_deref_posincr;
  "Test bool not" >:: test_bool_not;
  "Test mixed sorts" >:: test_mixed_sorts;
]

let () = match Bap_main.init () with
  | Ok () -> run_test_tt_main suite
  | Error e ->
    Format.eprintf "Failed to initialize BAP: %a"
      Bap_main.Extension.Error.pp e;
    exit 1
