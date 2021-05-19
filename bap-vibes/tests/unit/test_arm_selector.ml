open! Core_kernel
open Bap_knowledge
open Bap_vibes
open Bap_core_theory
module KB = Knowledge
open KB.Let
module H = Helpers
open OUnit2
open Theory

module Prog1 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [ v1 := var v2 + var v3 ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog2 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [ v1 := var v2 + (var v3 + var v1) ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog3 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [ v1 := var v2 << var v3 ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog4 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [ v1 := var v2 >> var v3 ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog5 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [ v1 := var v2 & var v3 ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog6 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define (Bitv.define 32) "v1" in
    let v2 = Var.define (Bitv.define 32) "v2" in
    let v3 = Var.define (Bitv.define 32) "v3" in
    let data = data_body [ v1 := var v2 |$ var v3 ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog7 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define Bool.t "v1" in
    let v2 = Var.define Bool.t "v2" in
    let v3 = Var.define Bool.t "v3" in
    let data = data_body [ v1 := var v2 && var v3 ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog8 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v1 = Var.define Bool.t "v1" in
    let v2 = Var.define Bool.t "v2" in
    let v3 = Var.define Bool.t "v3" in
    let data = data_body [ v1 := var v2 || var v3 ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog9 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let tgt = Bap.Std.Tid.for_name "tgt" in
    let data = data_body [] in
    let control = ctrl_body [ goto tgt ] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog10 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let b32 = Bitv.define 32 in
    let mem_ty = Mem.define b32 b32 in
    let v1 = Var.define b32 "v1" in
    let v2 = Var.define b32 "v2" in
    let mem = Var.define mem_ty "mem" in
    let data = data_body [ mem := store (var mem) (var v1) (var v2) ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog11 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let b32 = Bitv.define 32 in
    let mem_ty = Mem.define b32 b32 in
    let v1 = Var.define b32 "v1" in
    let v2 = Var.define b32 "v2" in
    let mem = Var.define mem_ty "mem" in
    let data = data_body [ v2 := load (var mem) (var v1) ] in
    let control = ctrl_body [] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Prog12 (S : Core) = struct
  open S

  open Core_notations.Make (S)

  let prog =
    let v = Var.define Bool.t "v" in
    let tgt = Bap.Std.Tid.for_name "tgt" in
    let data = data_body [] in
    let fall = perform Effect.Sort.fall in
    let control = ctrl_body [ branch (var v) (goto tgt) fall ] in
    let l = Bap.Std.Tid.for_name "entry" in
    blk l data control
end

module Arm = Arm_selector
module Prog1_inst = Prog1 (Arm.ARM_Core)
module Prog2_inst = Prog2 (Arm.ARM_Core)
module Prog3_inst = Prog3 (Arm.ARM_Core)
module Prog4_inst = Prog4 (Arm.ARM_Core)
module Prog5_inst = Prog5 (Arm.ARM_Core)
module Prog6_inst = Prog6 (Arm.ARM_Core)
module Prog7_inst = Prog7 (Arm.ARM_Core)
module Prog8_inst = Prog8 (Arm.ARM_Core)
module Prog9_inst = Prog9 (Arm.ARM_Core)
module Prog10_inst = Prog10 (Arm.ARM_Core)
module Prog11_inst = Prog11 (Arm.ARM_Core)
module Prog12_inst = Prog12 (Arm.ARM_Core)

let test_ir (_ : test_ctxt) (v : unit eff) (expected : string list) : unit =
  let computation =
    let* v = v in
    let v = Arm.effect v in
    let asm =
      v |> Option.map ~f:Arm.ir
      |> Option.map ~f:Ir.dummy_reg_alloc
      |> Option.map ~f:Arm.Pretty.arm_ir_pretty
      |> Option.map ~f:Result.ok (* We turn an [Error foo] into a [None] *)
      |> Option.join
      (* And we squash the Options *)
    in
    let* patch = KB.Object.create Data.Patch.patch in
    let* _ = Data.Patch.set_assembly patch asm in
    KB.return patch
  in
  let result = KB.run Data.Patch.patch computation KB.empty in
  let rexpected = List.map ~f:Str.regexp expected in
  let rexpected = Some rexpected in
  let cmp expected input =
    match (input, expected) with
    | Some input, Some expected ->
        let pairs = List.zip_exn expected input in
        let matches =
          List.map pairs ~f:(fun (pat, str) -> Str.string_match pat str 0)
        in
        List.for_all ~f:(fun b -> b) matches
    | _ -> false
  in
  let print_regex_list_opt _ = H.print_string_list_opt (Some expected) in
  H.assert_property ~cmp ~p_res:H.print_string_list_opt
    ~p_expected:print_regex_list_opt Data.Patch.assembly rexpected result

(* FIXME: we currently don't check to see if the variable names are
   consistent! *)
let test_ir1 ctxt =
  test_ir ctxt Prog1_inst.prog [ "entry:"; "add R0, R0, R0"; "mov R0, R0" ]

let test_ir2 ctxt =
  test_ir ctxt Prog2_inst.prog
    [ "entry:"; "add R0, R0, R0"; "add R0, R0, R0"; "mov R0, R0" ]

let test_ir3 ctxt =
  test_ir ctxt Prog3_inst.prog [ "entry:"; "lsl R0, R0, R0"; "mov R0, R0" ]

let test_ir4 ctxt =
  test_ir ctxt Prog4_inst.prog [ "entry:"; "lsr R0, R0, R0"; "mov R0, R0" ]

let test_ir5 ctxt =
  test_ir ctxt Prog5_inst.prog [ "entry:"; "and R0, R0, R0"; "mov R0, R0" ]

let test_ir6 ctxt =
  test_ir ctxt Prog6_inst.prog [ "entry:"; "orr R0, R0, R0"; "mov R0, R0" ]

let test_ir7 ctxt =
  test_ir ctxt Prog7_inst.prog [ "entry:"; "and R0, R0, R0"; "mov R0, R0" ]

let test_ir8 ctxt =
  test_ir ctxt Prog8_inst.prog [ "entry:"; "orr R0, R0, R0"; "mov R0, R0" ]

let test_ir9 ctxt = test_ir ctxt Prog9_inst.prog [ "entry:"; "b tgt" ]

let test_ir10 ctxt = test_ir ctxt Prog10_inst.prog [ "entry:"; "str R0, R0" ]

let test_ir11 ctxt =
  test_ir ctxt Prog11_inst.prog [ "entry:"; "ldr R0, \\[R0\\]"; "mov R0, R0" ]

let test_ir12 ctxt =
  test_ir ctxt Prog12_inst.prog
    [
      "entry:";
      "cmp R0, #0";
      "beq true_branch";
      "b false_branch";
      "true_branch:";
      "b tgt";
      "false_branch:";
    ]

let suite =
  [
    "Test Arm.ir 1" >:: test_ir1;
    "Test Arm.ir 2" >:: test_ir2;
    "Test Arm.ir 3" >:: test_ir3;
    "Test Arm.ir 4" >:: test_ir4;
    "Test Arm.ir 5" >:: test_ir5;
    "Test Arm.ir 6" >:: test_ir6;
    "Test Arm.ir 7" >:: test_ir7;
    "Test Arm.ir 8" >:: test_ir8;
    "Test Arm.ir 9" >:: test_ir9;
    "Test Arm.ir 10" >:: test_ir10;
    "Test Arm.ir 11" >:: test_ir11;
    "Test Arm.ir 12" >:: test_ir12;
  ]
