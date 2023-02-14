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

open Core
open Bap.Std
open OUnit2

module Ir = Vibes_ir.Types
module Selector = Vibes_select.Arm_selector

open Helpers
open Progs

let dummy_reg_alloc (t : Ir.t) : Ir.t = Ir.map_opvars t ~f:(fun v ->
    match v.preassign with
    | Some _ -> v
    | None ->
      let var = List.hd_exn v.temps in
      {v with preassign = Some (Var.create "R0" (Var.typ var))})

let test_ir = test_ir
    ~tgt:"bap:armv7+le"
    ~lang:"bap:llvm-armv7"
    ~select:(Selector.select ~is_thumb:false)
    ~dummy_reg_alloc

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

let test_ir16 (ctxt : test_ctxt) : unit = test_ir ctxt Prog16.prog [
    blk_pat ^ ":";
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
    "ldr R0, =393215";
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

let test_ir27 (ctxt : test_ctxt) : unit = test_ir ctxt Prog27.prog [
    blk_pat ^ ":";
    "mvn R0, #0";
  ]

let test_ir28 (ctxt : test_ctxt) : unit = test_ir ctxt Prog28.prog [
    blk_pat ^ ":";
    "movw R0, #4660";
    "ldrb R0, \\[R0, #0\\]"
  ]

let test_ir29 (ctxt : test_ctxt) : unit = test_ir ctxt Prog29.prog [
    blk_pat ^ ":";
    "movw R0, #4660";
    "mov R0, #5";
    "strb R0, \\[R0\\]"
  ]

let test_ir30 (ctxt : test_ctxt) : unit = test_ir ctxt Prog30.prog [
    blk_pat ^ ":";
    "mov R0, #1";
    "cmp R0, #42";
    "movlo R0, #0";
    "mov R0, R0";
  ]

let test_ir31 (ctxt : test_ctxt) : unit = test_ir ctxt Prog31.prog [
    blk_pat ^ ":";
    "ldrsb R0, \\[R0, #8\\]";
  ]

let test_ir32 (ctxt : test_ctxt) : unit = test_ir ctxt Prog32.prog [
    blk_pat ^ ":";
    "ldrsh R0, \\[R0, #8\\]";
  ]

let test_ir33 (ctxt : test_ctxt) : unit = test_ir ctxt Prog33.prog [
    blk_pat ^ ":";
    "ldr R0, \\[R0, R0, lsl #2\\]";
  ]

let suite : test = "Test ARM selector" >::: [
    "Test ARM 1" >:: test_ir1;
    "Test ARM 2" >:: test_ir2;
    "Test ARM 3" >:: test_ir3;
    "Test ARM 4" >:: test_ir4;
    "Test ARM 5" >:: test_ir5;
    "Test ARM 6" >:: test_ir6;
    "Test ARM 9" >:: test_ir9;
    "Test ARM 10" >:: test_ir10;
    "Test ARM 11" >:: test_ir11;
    "Test ARM 12" >:: test_ir12;
    "Test ARM 13" >:: test_ir13;
    "Test ARM 14" >:: test_ir14;
    "Test ARM 15" >:: test_ir15;
    "Test ARM 16" >:: test_ir16;
    "Test ARM 17" >:: test_ir17;
    "Test ARM 18" >:: test_ir18;
    "Test ARM 19" >:: test_ir19;
    "Test ARM 20" >:: test_ir20;
    "Test ARM 21" >:: test_ir21;
    "Test ARM 22" >:: test_ir22;
    "Test ARM 23" >:: test_ir23;
    "Test ARM 24" >:: test_ir24;
    "Test ARM 25" >:: test_ir25;
    "Test ARM 26" >:: test_ir26;
    "Test ARM 27" >:: test_ir27;
    "Test ARM 28" >:: test_ir28;
    "Test ARM 29" >:: test_ir29;
    "Test ARM 30" >:: test_ir30;
    "Test ARM 31" >:: test_ir31;
    "Test ARM 32" >:: test_ir32;
    "Test ARM 33" >:: test_ir33;
  ]

let () = match Bap_main.init () with
  | Ok () -> run_test_tt_main suite
  | Error e ->
    Format.eprintf "Failed to initialize BAP: %a"
      Bap_main.Extension.Error.pp e;
    exit 1
