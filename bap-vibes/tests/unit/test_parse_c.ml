open Bap_vibes
open OUnit2
open Core_kernel
open Bap_core_theory
open Bap_knowledge
open Knowledge.Syntax
open Knowledge.Let

let eff_to_str sem =
  Format.asprintf "%a" (KB.Value.pp_slots ["bap:bil"]) sem |>
  String.filter ~f:(fun c -> not Char.(c = '\"'))

(* Our "golden inputs" compared ignoring whitespace. *)
let compare_sem sem str =
  let strip s = String.filter s ~f:(Fn.non Char.is_whitespace) in
  String.equal (strip sem) (strip str)

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
          let sem_str = eff_to_str sem in
          KB.return @@
          assert_equal ~cmp:compare_sem ~printer:ident sem_str s2
        end


let test_var_decl _ = assert_parse_eq "int x, y, z;" "(())"

let test_assign _ = assert_parse_eq "int x, y; x = y;" "{ x := y }"

let test_seq _ = assert_parse_eq "int x, y, z; x = y; y = z;" "{ x := y y := z }"

let test_ite _ =
  assert_parse_eq
    "int cond_expr; if(cond_expr){goto l1;}else{goto l2;};"
    "{
     if (cond_expr) {
      bap:call(l1)
     } else {
       bap:call(l2)
      }
     }"

let test_fallthrough _ =
  assert_parse_eq
    "int x, y, z; if (x) { goto l; } x = z; "
    "{
   if (x) {
     bap:call(l)
   }
   x := z
   }"

let test_array _ =
  assert_parse_eq
    "int a, y; y = a[7];"
    "{ y := mem[a + 7, el]:u32 }"

let test_compound _ = assert_parse_eq
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
      bap:call(fred)
    } else {
      bap:call(larry)
     }
    }"

let test_call_hex _ = assert_parse_eq
  "int temp;
   if (temp == 0)
    { (0x3ec)(); }"
  "{
   if (temp = 0) {
     jmp 0x3EC
   }
 }"

let test_load_short _ = assert_parse_eq
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
  "Test load short" >:: test_load_short;
]
