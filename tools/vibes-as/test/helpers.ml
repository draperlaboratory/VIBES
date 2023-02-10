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
open Bap_core_theory
open OUnit2

module Ir = Vibes_ir.Types
module Asm = Vibes_as.Types.Assembly
module Patch_info = Vibes_patch_info.Types

open KB.Syntax

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
    (expected : string list)
    ~(tgt : string)
    ~(lang : string)
    ~(select : sub term -> Ir.t KB.t)
    ~(dummy_reg_alloc : Ir.t -> Ir.t) : unit =
  let state = Toplevel.current () in
  Toplevel.reset ();
  let result = Toplevel.var "select-arm" in
  let sub = sub () in
  Toplevel.put result begin
    let target = Theory.Target.of_string tgt in
    let language = Theory.Language.of_string lang in
    let+ ir = select sub in
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

let blk_pat : string = "blk\\([0-9]\\|[a-f]\\)*"
