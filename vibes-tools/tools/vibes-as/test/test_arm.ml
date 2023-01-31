open Core
open Bap.Std
open Bap_core_theory
open OUnit2

open KB.Syntax

module Ir = Vibes_ir.Types
module Asm = Vibes_as.Types.Assembly
module Selector = Vibes_select.Arm_selector
module Patch_info = Vibes_patch_info.Types

let dummy_reg_alloc (t : Ir.t) : Ir.t = Ir.map_opvars t ~f:(fun v ->
    match v.preassign with
    | Some _ -> v
    | None ->
      let var = List.hd_exn v.temps in
      {v with preassign = Some (Var.create "R0" (Var.typ var))})

let compare_result
    (expected : string list option)
    (input : string list option) : bool =
  let rexpected = Option.map expected ~f:(List.map ~f:Str.regexp) in
  match input, rexpected with
  | None, _ | _ , None -> false
  | Some input, Some expected ->
    match List.zip expected input with
    | Unequal_lengths -> false
    | Ok pairs ->
      List.map pairs ~f:(fun (pat, str) ->
          Str.string_match pat str 0) |>
      List.for_all ~f:Fn.id

let print_opt_str_list : string list option -> string = function
  | Some l -> List.to_string l ~f:Fn.id
  | None -> "None"

let dummy_patch_info : Patch_info.t = {
  patch_point = Word.of_string "0x1234:32";
  patch_size = 0L;
  sp_align = 0;
  patch_vars = [];
}

let test_ir
    (_ : test_ctxt)
    (sub : unit -> sub term)
    (expected : string list) : unit =
  let state = Toplevel.current () in
  Toplevel.reset ();
  let result = Toplevel.var "select-arm" in
  let sub = sub () in
  Toplevel.put result begin
    let target = Theory.Target.of_string "bap:armv7+le" in
    let language = Theory.Language.of_string "bap:llvm-armv7" in
    let+ ir = Selector.select sub ~is_thumb:false in
    let ir = dummy_reg_alloc ir in
    let printer = match Vibes_as.Utils.asm_printer target language with
      | Ok p -> p
      | Error e ->
        failwith @@
        Format.asprintf "Failed to get ASM printer: %a" KB.Conflict.pp e in
    printer ir dummy_patch_info |> Result.ok |> Option.map ~f:(fun asm ->
        Asm.blocks asm |> List.concat_map ~f:(fun blk ->
            (Asm.label blk ^ ":") :: Asm.insns blk))
  end;
  let result = Toplevel.get result in
  Toplevel.set state;
  assert_equal (Some expected) result
    ~cmp:compare_result
    ~printer:print_opt_str_list


let v1 : var = Var.create "v1" (Imm 32)
let v2 : var = Var.create "v2" (Imm 32)
let v3 : var = Var.create "v3" (Imm 32)
let v : var = Var.create "v" (Imm 1)
let func () : tid = Tid.for_name "some_function"
let mem : var = Var.create "mem" (Mem (`r32, `r8))
let (!!) (i : int) : exp = Bil.int (Word.of_int ~width:32 i)

let add_goto (sub : sub term) (tgt : tid) : sub term =
  Term.map blk_t sub ~f:(fun blk ->
      let blk = Blk.Builder.init blk in
      Blk.Builder.add_jmp blk @@ Jmp.create @@ Goto (Label.direct tgt);
      Blk.Builder.result blk)

let add_call (sub : sub term) (tgt : tid) : sub term =
  Term.map blk_t sub ~f:(fun blk ->
      let return_blk = Blk.Builder.create () |> Blk.Builder.result in
      let blk = Blk.Builder.init blk in
      let call = Call.create ()
          ~return:(Label.direct @@ Term.tid return_blk)
          ~target:(Label.direct tgt) in
      Blk.Builder.add_jmp blk @@ Jmp.create @@ Call call;
      Blk.Builder.result blk)

module Prog1 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 + var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog2 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 + (var v3 + var v1)] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog3 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 lsl var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog4 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 lsr var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog5 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 land var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog6 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 lor var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog9 = struct

  let prog () : sub term =
    let tgt = Bap.Std.Tid.for_name "tgt" in
    let bil = [] in
    let prog = Bap_wp.Bil_to_bir.bil_to_sub bil in
    add_goto prog tgt

end

module Prog10 = struct

  let prog () : sub term =
    let bil = Bil.[mem := store ~mem:(var mem) ~addr:(var v1) (var v2) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog11 = struct

  let prog () : sub term =
    let bil = Bil.[v2 := load ~mem:(var mem) ~addr:(var v1) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog12 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (var v) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog13 = struct

  let prog () : sub term =
    let bil = Bil.[v2 := load ~mem:(var mem) ~addr:(var v1) BigEndian `r16] in
    let res = Bap_wp.Bil_to_bir.bil_to_sub bil in
    res

end

module Prog14 = struct

  let prog () =
    let bil = [] in
    let prog = Bap_wp.Bil_to_bir.bil_to_sub bil in
    add_call prog @@ func ()

end

module Prog15 = struct


  let prog () : sub term =
    let bil = Bil.[v1 := var v2 + !!42] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog16 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog17 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := !!5000] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog18 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (var v <> !!0) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog19 = struct

  let prog () : sub term =
    let bil = Bil.[mem := store ~mem:(var mem) ~addr:(var v1 + !!8) (var v2) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog20 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 * !!5] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog21 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := var v2 * !!8] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog22 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := !!0x1FFFF] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog23 = struct

  let prog () : sub term =
    let bil = Bil.[v1 := lnot @@ var v2; v2 := unop neg @@ var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog24 = struct

  let prog () : sub term =
    let bil = Bil.[
        v1 := !!1 lsl !!3;
        v2 := !!1 lsl var v1;
        v3 := var v2 lor !!6;
      ] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog25 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (var v1 > !!42) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog26 = struct

  let prog () : sub term =
    let bil = Bil.[if_ (lnot (var v)) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

let blk_pat : string = "blk\\([0-9]\\|[a-f]\\)*"

let test_ir1 (ctxt : test_ctxt) : unit = test_ir ctxt Prog1.prog [
    blk_pat ^ ":";
    "add R0, R0, R0";
  ]

let test_ir2 (ctxt : test_ctxt) : unit = test_ir ctxt Prog2.prog [
    blk_pat ^ ":";
    "add R0, R0, R0";
    "add R0, R0, R0";
  ]

let test_ir3 (ctxt : test_ctxt) : unit = test_ir ctxt Prog3.prog [
    blk_pat ^ ":";
    "lsl R0, R0, R0";
  ]

let test_ir4 (ctxt : test_ctxt) : unit = test_ir ctxt Prog4.prog [
    blk_pat ^ ":";
    "lsr R0, R0, R0";
  ]

let test_ir5 (ctxt : test_ctxt) : unit = test_ir ctxt Prog5.prog [
    blk_pat ^ ":";
    "and R0, R0, R0";
  ]

let test_ir6 (ctxt : test_ctxt) : unit = test_ir ctxt Prog6.prog [
    blk_pat ^ ":";
    "orr R0, R0, R0";
  ]

let test_ir9 (ctxt : test_ctxt) : unit = test_ir ctxt Prog9.prog [
    blk_pat ^ ":";
    "b tgt";
  ]

let test_ir10 (ctxt : test_ctxt) : unit = test_ir ctxt Prog10.prog [
    blk_pat ^ ":";
    "str R0, \\[R0\\]"
  ]

let test_ir11 (ctxt : test_ctxt) : unit = test_ir ctxt Prog11.prog [
    blk_pat ^ ":";
    "ldr R0, \\[R0\\]";
  ]

let test_ir12 (ctxt : test_ctxt) : unit = test_ir ctxt Prog12.prog [
    blk_pat ^ ":";
    "cmp R0, #0";
    "bne " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let test_ir13 (ctxt : test_ctxt) : unit = test_ir ctxt Prog13.prog [
    blk_pat ^ ":";
    "ldrh R0, \\[R0\\]";
  ]

let test_ir14 (ctxt : test_ctxt) : unit = test_ir ctxt Prog14.prog [
    blk_pat ^ ":";
    "bl some_function";
    "b " ^ blk_pat;
  ]

let test_ir15 (ctxt : test_ctxt) : unit = test_ir ctxt Prog15.prog [
    blk_pat ^ ":";
    "add R0, R0, #42";
  ]

let test_ir16 (ctxt : test_ctxt) : unit = test_ir ctxt Prog16.prog
    [blk_pat ^ ":";
     "mov R0, R0";
    ]

let test_ir17 (ctxt : test_ctxt) : unit = test_ir ctxt Prog17.prog [
    blk_pat ^ ":";
    "movw R0, #5000";
  ]

let test_ir18 (ctxt : test_ctxt) : unit = test_ir ctxt Prog18.prog [
    blk_pat ^ ":";
    "cmp R0, #0";
    "bne " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let test_ir19 (ctxt : test_ctxt) : unit = test_ir ctxt Prog19.prog [
    blk_pat ^ ":";
    "str R0, \\[R0, #8\\]";
  ]

let test_ir20 (ctxt : test_ctxt) : unit = test_ir ctxt Prog20.prog [
    blk_pat ^ ":";
    "mov R0, #5";
    "mul R0, R0, R0";
  ]

let test_ir21 (ctxt : test_ctxt) : unit = test_ir ctxt Prog21.prog [
    blk_pat ^ ":";
    "lsl R0, R0, #3";
  ]

let test_ir22 (ctxt : test_ctxt) : unit = test_ir ctxt Prog22.prog [
    blk_pat ^ ":";
    "ldr R0, =131071";
  ]

let test_ir23 (ctxt : test_ctxt) : unit = test_ir ctxt Prog23.prog [
    blk_pat ^ ":";
    "mvn R0, R0";
    "neg R0, R0"
  ]

let test_ir24 (ctxt : test_ctxt) : unit = test_ir ctxt Prog24.prog [
    blk_pat ^ ":";
    "mov R0, #8";
    "mov R0, #1";
    "lsl R0, R0, R0";
    "mov R0, #6";
    "orr R0, R0, R0";
  ]

let test_ir25 (ctxt : test_ctxt) : unit = test_ir ctxt Prog25.prog [
    blk_pat ^ ":";
    "cmp R0, #42";
    "bhi " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let test_ir26 (ctxt : test_ctxt) : unit = test_ir ctxt Prog26.prog [
    blk_pat ^ ":";
    "cmp R0, #0";
    "beq " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "mov R0, #3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let suite : test = "Test ARM selector" >::: [
    "Test Arm.ir 1" >:: test_ir1;
    "Test Arm.ir 2" >:: test_ir2;
    "Test Arm.ir 3" >:: test_ir3;
    "Test Arm.ir 4" >:: test_ir4;
    "Test Arm.ir 5" >:: test_ir5;
    "Test Arm.ir 6" >:: test_ir6;
    "Test Arm.ir 9" >:: test_ir9;
    "Test Arm.ir 10" >:: test_ir10;
    "Test Arm.ir 11" >:: test_ir11;
    "Test Arm.ir 12" >:: test_ir12;
    "Test Arm.ir 13" >:: test_ir13;
    "Test Arm.ir 14" >:: test_ir14;
    "Test Arm.ir 15" >:: test_ir15;
    "Test Arm.ir 16" >:: test_ir16;
    "Test Arm.ir 17" >:: test_ir17;
    "Test Arm.ir 18" >:: test_ir18;
    "Test Arm.ir 19" >:: test_ir19;
    "Test Arm.ir 20" >:: test_ir20;
    "Test Arm.ir 21" >:: test_ir21;
    "Test Arm.ir 22" >:: test_ir22;
    "Test Arm.ir 23" >:: test_ir23;
    "Test Arm.ir 24" >:: test_ir24;
    "Test Arm.ir 25" >:: test_ir25;
    "Test Arm.ir 26" >:: test_ir26;
  ]

let () = match Bap_main.init () with
  | Ok () -> run_test_tt_main suite
  | Error e ->
    Format.eprintf "Failed to initialize BAP: %a"
      Bap_main.Extension.Error.pp e;
    exit 1
