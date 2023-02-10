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
module Selector = Vibes_select.Ppc_selector

open Helpers
open Progs

let dummy_reg_alloc (t : Ir.t) : Ir.t = Ir.map_opvars t ~f:(fun v ->
    match v.preassign with
    | Some _ -> v
    | None ->
      let var = List.hd_exn v.temps in
      {v with preassign = Some (Var.create "R3" (Var.typ var))})

let test_ir = test_ir
    ~tgt:"bap:powerpc"
    ~lang:"bap:llvm-powerpc32"
    ~select:Selector.select
    ~dummy_reg_alloc

let test_ir1 (ctxt : test_ctxt) : unit = test_ir ctxt Prog1.prog [
    blk_pat ^ ":";
    "add 3, 3, 3";
  ]

let test_ir2 (ctxt : test_ctxt) : unit = test_ir ctxt Prog2.prog [
    blk_pat ^ ":";
    "add 3, 3, 3";
    "add 3, 3, 3";
  ]

let test_ir3 (ctxt : test_ctxt) : unit = test_ir ctxt Prog3.prog [
    blk_pat ^ ":";
    "slw 3, 3, 3";
  ]

let test_ir4 (ctxt : test_ctxt) : unit = test_ir ctxt Prog4.prog [
    blk_pat ^ ":";
    "srw 3, 3, 3";
  ]

let test_ir5 (ctxt : test_ctxt) : unit = test_ir ctxt Prog5.prog [
    blk_pat ^ ":";
    "and 3, 3, 3";
  ]

let test_ir6 (ctxt : test_ctxt) : unit = test_ir ctxt Prog6.prog [
    blk_pat ^ ":";
    "or 3, 3, 3";
  ]

let test_ir9 (ctxt : test_ctxt) : unit = test_ir ctxt Prog9.prog [
    blk_pat ^ ":";
    "b tgt";
  ]

let test_ir10 (ctxt : test_ctxt) : unit = test_ir ctxt Prog10.prog [
    blk_pat ^ ":";
    "stw 3, 0(3)"
  ]

let test_ir11 (ctxt : test_ctxt) : unit = test_ir ctxt Prog11.prog [
    blk_pat ^ ":";
    "lwz 3, 0(3)";
  ]

let test_ir12 (ctxt : test_ctxt) : unit = test_ir ctxt Prog12.prog [
    blk_pat ^ ":";
    "cmpwi 7, 3, 0";
    "bne 7, " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let test_ir13 (ctxt : test_ctxt) : unit = test_ir ctxt Prog13.prog [
    blk_pat ^ ":";
    "lhz 3, 0(3)";
  ]

let test_ir14 (ctxt : test_ctxt) : unit = test_ir ctxt Prog14.prog [
    blk_pat ^ ":";
    "bl some_function";
    "b " ^ blk_pat;
  ]

let test_ir15 (ctxt : test_ctxt) : unit = test_ir ctxt Prog15.prog [
    blk_pat ^ ":";
    "addi 3, 3, 42";
  ]

let test_ir16 (ctxt : test_ctxt) : unit = test_ir ctxt Prog16.prog [
    blk_pat ^ ":";
    "mr 3, 3";
  ]

let test_ir17 (ctxt : test_ctxt) : unit = test_ir ctxt Prog17.prog [
    blk_pat ^ ":";
    "li 3, 5000";
  ]

let test_ir18 (ctxt : test_ctxt) : unit = test_ir ctxt Prog18.prog [
    blk_pat ^ ":";
    "cmpwi 7, 3, 0";
    "bne 7, " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let test_ir19 (ctxt : test_ctxt) : unit = test_ir ctxt Prog19.prog [
    blk_pat ^ ":";
    "stw 3, 8(3)";
  ]

let test_ir20 (ctxt : test_ctxt) : unit = test_ir ctxt Prog20.prog [
    blk_pat ^ ":";
    "mulli 3, 3, 5";
  ]

let test_ir21 (ctxt : test_ctxt) : unit = test_ir ctxt Prog21.prog [
    blk_pat ^ ":";
    "slwi 3, 3, 3";
  ]

let test_ir22 (ctxt : test_ctxt) : unit = test_ir ctxt Prog22.prog [
    blk_pat ^ ":";
    "lis 3, 1";
    "ori 3, 3, 65535";
  ]

let test_ir23 (ctxt : test_ctxt) : unit = test_ir ctxt Prog23.prog [
    blk_pat ^ ":";
    "not 3, 3";
    "neg 3, 3";
  ]

let test_ir24 (ctxt : test_ctxt) : unit = test_ir ctxt Prog24.prog [
    blk_pat ^ ":";
    "li 3, 8";
    "li 3, 1";
    "slw 3, 3, 3";
    "ori 3, 3, 6";
  ]

let test_ir25 (ctxt : test_ctxt) : unit = test_ir ctxt Prog25.prog [
    blk_pat ^ ":";
    "cmplwi 7, 3, 42";
    "bgt 7, " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let test_ir26 (ctxt : test_ctxt) : unit = test_ir ctxt Prog26.prog [
    blk_pat ^ ":";
    "cmpwi 7, 3, 0";
    "beq 7, " ^ blk_pat;
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 4";
    "b " ^ blk_pat;
    blk_pat ^ ":";
    "li 3, 3";
    "b " ^ blk_pat;
    blk_pat ^ ":";
  ]

let test_ir27 (ctxt : test_ctxt) : unit = test_ir ctxt Prog27.prog [
    blk_pat ^ ":";
    "li 3, 4294967295";
  ]

let test_ir28 (ctxt : test_ctxt) : unit = test_ir ctxt Prog28.prog [
    blk_pat ^ ":";
    "li 3, 4660";
    "lbz 3, 0(3)"
  ]

let test_ir29 (ctxt : test_ctxt) : unit = test_ir ctxt Prog29.prog [
    blk_pat ^ ":";
    "li 3, 5";
    "li 3, 4660";
    "stb 3, 0(3)"
  ]

let test_ir30 (ctxt : test_ctxt) : unit = test_ir ctxt Prog30.prog [
    blk_pat ^ ":";
    "cmplwi 7, 3, 42";
    "mfcr 3";
    "rlwinm 3, 3, 29, 31, 31";
    "xori 3, 3, 1"
  ]

let test_ir31 (ctxt : test_ctxt) : unit = test_ir ctxt Prog31.prog [
    blk_pat ^ ":";
    "lbz 3, 8(3)";
    "extsb 3, 3";
  ]

let test_ir32 (ctxt : test_ctxt) : unit = test_ir ctxt Prog32.prog [
    blk_pat ^ ":";
    "lha 3, 8(3)";
  ]

let test_ir33 (ctxt : test_ctxt) : unit = test_ir ctxt Prog33.prog [
    blk_pat ^ ":";
    "slwi 3, 3, 2";
    "lwzx 3, 3, 3";
  ]

let suite : test = "Test PPC selector" >::: [
    "Test PPC 1" >:: test_ir1;
    "Test PPC 2" >:: test_ir2;
    "Test PPC 3" >:: test_ir3;
    "Test PPC 4" >:: test_ir4;
    "Test PPC 5" >:: test_ir5;
    "Test PPC 6" >:: test_ir6;
    "Test PPC 9" >:: test_ir9;
    "Test PPC 10" >:: test_ir10;
    "Test PPC 11" >:: test_ir11;
    "Test PPC 12" >:: test_ir12;
    "Test PPC 13" >:: test_ir13;
    "Test PPC 14" >:: test_ir14;
    "Test PPC 15" >:: test_ir15;
    "Test PPC 16" >:: test_ir16;
    "Test PPC 17" >:: test_ir17;
    "Test PPC 18" >:: test_ir18;
    "Test PPC 19" >:: test_ir19;
    "Test PPC 20" >:: test_ir20;
    "Test PPC 21" >:: test_ir21;
    "Test PPC 22" >:: test_ir22;
    "Test PPC 23" >:: test_ir23;
    "Test PPC 24" >:: test_ir24;
    "Test PPC 25" >:: test_ir25;
    "Test PPC 26" >:: test_ir26;
    "Test PPC 27" >:: test_ir27;
    "Test PPC 28" >:: test_ir28;
    "Test PPC 29" >:: test_ir29;
    "Test PPC 30" >:: test_ir30;
    "Test PPC 31" >:: test_ir31;
    "Test PPC 32" >:: test_ir32;
    "Test PPC 33" >:: test_ir33;
  ]

let () = match Bap_main.init () with
  | Ok () -> run_test_tt_main suite
  | Error e ->
    Format.eprintf "Failed to initialize BAP: %a"
      Bap_main.Extension.Error.pp e;
    exit 1
