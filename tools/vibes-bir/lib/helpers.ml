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

open KB.Syntax

module T = Theory

let create_sub
    (name : string)
    (blks : blk term list) : sub term KB.t =
  if List.is_empty blks then
    KB.fail @@ Errors.No_blks
      "Vibes_bir.Helpers.create_sub: got an empty list of blks"
  else
    let+ tid = Theory.Label.fresh in
    Sub.create ~name ~blks ~tid ()

let no_jmps (blk : blk term) : bool =
  Seq.is_empty @@ Term.enum jmp_t blk

let is_unconditional (jmp : jmp term) : bool =
  match Jmp.cond jmp with
  | Int _ -> true
  | _ -> false

let is_call (jmp : jmp term) : bool = match Jmp.kind jmp with
  | Call _ -> true
  | _ -> false

let is_indirect_label : label -> bool = function
  | Indirect _ -> true
  | Direct _ -> false

let is_exit_call (jmp : jmp term) : bool = match Jmp.kind jmp with
  | Call call ->
    Call.return call |>
    Option.value_map ~default:true ~f:is_indirect_label
  | _ -> false

let exit_blks (sub : sub term) : (blk term list, KB.conflict) result =
  match Term.enum blk_t sub |> Seq.to_list with
  | [] ->
    let msg = "Vibes_bir.Helpers.exit_blks: got an empty list of blks" in
    Error (Errors.No_blks msg)
  | [_] as blks -> Ok blks
  | _ ->
    let cfg = Sub.to_cfg sub in
    let nodes_seq = Graphs.Ir.nodes cfg in
    let nodes = Seq.to_list nodes_seq in
    Result.return @@ List.filter_map nodes ~f:(fun node ->
        let blk = Graphs.Ir.Node.label node in
        if Graphs.Ir.Node.degree node cfg ~dir:`Out = 0 then Some blk
        else match Term.enum jmp_t blk |> Seq.to_list with
          | [] -> Some blk
          | [jmp] when not @@ is_unconditional jmp -> Some blk
          | jmps -> Option.some_if (List.exists jmps ~f:is_exit_call) blk)

let has_call (blk : blk term) : bool =
  Term.enum jmp_t blk |> Seq.exists ~f:is_call

let call_blks (sub : sub term) : blk term list =
  Term.enum blk_t sub |> Seq.to_list |>
  List.filter_map ~f:(fun blk -> Option.some_if (has_call blk) blk)

let entry_blk : sub term -> (blk term, KB.conflict) result =
  let error =
    Errors.No_blks "Vibes_bir.Helpers.entry_blk: got an empty list of blks" in
  fun sub -> Term.first blk_t sub |> Result.of_option ~error

let entry_tid (sub : sub term) : (tid, KB.conflict) result =
  entry_blk sub |> Result.map ~f:Term.tid

(* This was borrowed from `bap/lib/bap_types/bap_var.ml`. Perhaps
   it should be exposed in the user-facing API? *)

let unknown =
  let package = Vibes_constants.Bap_kb.package in
  let unknown =
    Theory.Value.Sort.Name.declare ~package "Unknown" in
  Theory.Value.Sort.sym unknown

let sort_of_typ t =
  let ret = T.Value.Sort.forget in
  match t with
  | Type.Imm 1 -> ret T.Bool.t
  | Type.Imm m -> ret @@ T.Bitv.define m
  | Type.Mem (ks,vs) ->
    let ks,vs = Size.(in_bits ks, in_bits vs) in
    let ks,vs = T.Bitv.(define ks, define vs) in
    ret @@ T.Mem.define ks vs
  | Type.Unk -> ret @@ unknown
