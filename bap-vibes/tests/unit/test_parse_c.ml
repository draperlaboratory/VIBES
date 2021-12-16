open Core_kernel
open Bap_core_theory
open KB.Let
open Bap_vibes
open OUnit2

let eff_to_str sem =
  Format.asprintf "%a" (KB.Value.pp_slots ["bap:bil"]) sem |>
  String.filter ~f:(fun c -> not Char.(c = '\"'))

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
        let sem_str = eff_to_str sem in
        KB.return @@ assert_equal ~cmp:compare_sem ~printer:ident s2 sem_str
      end

let test_var_decl _ =
  assert_parse_eq
    "int x, y, z;"
    "(())"

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
    "int a, y; y = a[7];"
    "{ y := mem[a + 7, el]:u32 }"

let test_compound _ =
  assert_parse_eq
    "int x, y, z;
     char q;
     x = 0x7;
     x = *y;
     if((signed int)x > (signed int)0){
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
     if (temp == 0) { (0x3ec)(); }"
    "{
       if (temp = 0) {
         jmp 0x3EC
       }
     }"

let test_call_args_1 _ =
  assert_parse_eq
    "int a, b, c, f; f(a, b, c);"
    "{
       R0 := a
       R1 := b
       R2 := c
       call(f)
     }"

let test_call_args_2 _ =
  assert_parse_eq
    "int a, c, f; f(a, 0x1234, c);"
    "{
       R0 := a
       R1 := 0x1234
       R2 := c
       call(f)
     }"

let test_call_args_ret _ =
  assert_parse_eq
    "int a, b, c, d, f; d = f(a, b, c);"
    "{
       R0 := a
       R1 := b
       R2 := c
       call(f)
       d := R0
     }"

let test_call_args_ret_store _ =
  assert_parse_eq
    "int a, b, c, f;
     int *d;
     d = (int*)(a + 0x1337);
     *d = f(a, b, c);"
    "{
       d := a + 0x1337
       R0 := a
       R1 := b
       R2 := c
       call(f)
       mem := mem with [d, el]:u32 <- R0
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
    "int a, b, c, d, f;
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
    "{ x := mem[y, el]:u16 }"

let suite = [
  "Test vardecls" >:: test_var_decl;
  "Test assignment" >:: test_assign;
  "Test seq" >:: test_seq;
  "Test ite" >:: test_ite;
  "Test array" >:: test_array;
  "Test fallthrough" >:: test_fallthrough;
  "Test compound" >:: test_compound;
  "Test call hex" >:: test_call_hex;
  "Test call args 1" >:: test_call_args_1;
  "Test call args 2" >:: test_call_args_2;
  "Test call args ret" >:: test_call_args_ret;
  "Test call args ret store" >:: test_call_args_ret_store;
  "Test call args addrof" >:: test_call_args_addrof;
  "Test load short" >:: test_load_short;
]
