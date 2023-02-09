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

module Constants = Vibes_constants.Asm
module Ir = Vibes_ir.Types
module Ops = Vibes_select.Arm_ops
module Asm = Types.Assembly
module Patch_info = Vibes_patch_info.Types

type opc = (string, string * string) Either.t

let (let*) x f = Result.bind x ~f
let (let+) x f = Result.map x ~f

let fail msg = Result.fail @@ Errors.Printer_error msg

let tid_to_asm_label (t : tid) : string =
  let drop c = Char.(c = '%' || c = '@') in
  let name = Tid.name t |> String.strip ~drop in
  if Char.is_digit name.[0] then "blk" ^ name else name

let operand
    ?(is_loc : bool = false)
    (op : string)
    (o : Ir.Operand.t)
    (i : int) : (string, KB.Conflict.t) result =
  let+ p =
    match o with
    | Var v ->
      begin match v.preassign with
        | None -> fail "operand: operand.pre_assign field is empty"
        | Some v ->
          let s = Var.to_string v in
          if String.is_prefix s ~prefix:"R" then
            Ok (String.drop_prefix s 1)
          else if String.is_prefix s ~prefix:"CR" then
            Ok (String.drop_prefix s 2)
          else Ok s
      end
    | Const w -> Ok (Format.asprintf "%a" Word.pp_dec w)
    | Label l -> Ok (tid_to_asm_label l)
    | Void _ -> fail "operand: tried printing a Void operand!"
    | Offset c ->
      Ok (Format.asprintf "(%s + %a - %s)"
            Constants.patch_start_label
            Word.pp_dec c
            Constants.patch_location) in
  if is_loc then Format.asprintf "(%s)" p else p

let rm_void_args : Ir.Operand.t list -> Ir.Operand.t list =
  List.filter ~f:(function
      | Ir.Operand.Void _ -> false
      | _ -> true)

let is_const : Ir.Operand.t -> bool = function
  | Const _ -> true
  | _ -> false

let mk_loc_list (op : string) (args : Ir.Operand.t list) : bool list =
  match op with
  | "lbz" | "lha" | "lhz" | "lwz" | "stb" | "sth" | "stw" ->
    let len = List.length args in
    List.mapi args ~f:(fun i _ -> i = len - 1)
  | _ -> List.map args ~f:(fun _ -> false)

let operands
    (op : string)
    (lhs : Ir.Operand.t list)
    (rhs : Ir.Operand.t list) : (string, KB.conflict) result =
  (* `bl` may have pseudo-arguments. *)
  let* rhs =
    if String.(op = "bl") then match rhs with
      | x :: _ -> Ok [x]
      | _ ->
        fail "operands: expected at least 1 argument \
              for the `bl` opcode."
    else Ok rhs in
  (* `cmp` may have pseudo-arguments. *)
  let* rhs =
    if String.is_prefix op ~prefix:"cmp" then match rhs with
      | x :: y :: _ -> Ok [x; y]
      | _ ->
        fail @@ Format.sprintf
          "operands: expected at least 2 arguments \
           for the `%s` opcode." op
    else Ok rhs in
  (* `mfcr` may have pseudo-arguments. *)
  let* rhs =
    if String.(op = "mfcr") then match rhs with
      | [x] -> Ok []
      | _ ->
        fail "operands: expected 1 argument for the `mfcr` opcode"
    else Ok rhs in
  let l = rm_void_args (lhs @ rhs) in
  mk_loc_list op l |> List.zip_exn l |>
  List.mapi ~f:(fun i (o, is_loc) -> operand op o i ~is_loc) |>
  Result.all |> Result.map ~f:(fun s ->
      List.intersperse s ~sep:", " |>
      String.concat)

let operation (t : Ir.Operation.t) : (string, KB.conflict) result =
  let op = List.hd_exn t.opcodes in
  let+ ops = operands op t.lhs t.operands in
  Format.sprintf "%s %s" op ops

let block (t : Ir.Block.t) : (Types.Assembly.block, KB.conflict) result =
  let+ insns = List.map ~f:operation (t.data @ t.ctrl) |> Result.all in
  let label = Format.asprintf "%s" @@ tid_to_asm_label t.tid in
  Asm.Fields_of_block.create ~label ~insns

let ir : Asm.printer = fun t patch_info ->
  let+ blocks = List.map t.blks ~f:block |> Result.all in
  let Patch_info.{patch_point; patch_size; _} = patch_info in
  let patch_point = Bitvec.to_int64 @@ Word.to_bitvec patch_point in
  Asm.Fields.create ~patch_point ~patch_size ~directives:[] ~blocks
