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

(** Implements {!Linear_ssa}. *)

open Core_kernel
open Bap.Std
open Bap_core_theory

open KB.Let

(* Use the tid of the blk as the prefix, dropping the '%' at
   the beginning. *)
let prefix_from (blk : Blk.t) : string =
  let tid = Term.tid blk in
  let tid_str = Tid.to_string tid in
  String.drop_prefix tid_str 1

let linearize ~prefix:(prefix : string) (var : Var.t) : Var.t =
  let name = Var.name var in
  let typ = Var.typ var in
  let new_name = prefix ^ "_" ^ name in
  let escaped_name =
    String.substr_replace_all new_name ~pattern:"." ~with_:"_" in
  Var.create escaped_name typ

(* The prefix consists of an underscore (1 char), followed by the
   tid string (8 chars), ending with another underscore (1 char). *)
let prefix_len = 10

let orig_name (name : string) : string =
  let name = String.drop_prefix name prefix_len in
  let name, is_reg =
    Substituter.unmark_reg_name name |>
    Option.value_map ~default:(name, false) ~f:(fun name ->
        name, true) in
  let name = match String.split name ~on:'_' with
    | [] -> name
    | [name] when not @@ String.is_empty name -> name
    | [name; _] -> name
    | _ -> failwith @@ sprintf "Unexpected name pattern: %s" name in
  if is_reg then Substituter.mark_reg_name name else name

let same (a : var) (b : var) : bool =
  let a = Var.name a and b = Var.name b in
  String.equal a b || begin try
      String.equal (orig_name a) (orig_name b)
    with _ -> false
  end

let congruent (a : var) (b : var) : bool =
  let name_1 = String.drop_prefix (Var.name a) prefix_len in
  let name_2 = String.drop_prefix (Var.name b) prefix_len in
  (* We have to be careful feeding in vars which don't fit our naming
     convention for linear SSA. In particular, the instruction selector
     may generate additional temporary variables, which happens after
     we've run the linear SSA pass. *)
  if String.is_empty name_1 && String.is_empty name_2 then false
  else String.equal name_1 name_2

let rec linearize_exp ~(prefix : string) (exp : Bil.exp) : Bil.exp =
  match exp with
  | Bil.Load (sub_exp_1, sub_exp_2, endian, size) ->
    let new_sub_exp_1 = linearize_exp sub_exp_1 ~prefix in
    let new_sub_exp_2 = linearize_exp sub_exp_2 ~prefix in
    Bil.Load (new_sub_exp_1, new_sub_exp_2, endian, size) 
  | Bil.Store (sub_exp_1, sub_exp_2, sub_exp_3, endian, size) ->
    let new_sub_exp_1 = linearize_exp sub_exp_1 ~prefix in
    let new_sub_exp_2 = linearize_exp sub_exp_2 ~prefix in
    let new_sub_exp_3 = linearize_exp sub_exp_3 ~prefix in
    Bil.Store (new_sub_exp_1, new_sub_exp_2, new_sub_exp_3, endian, size)
  | Bil.UnOp (unop, sub_exp) -> 
    let new_sub_exp = linearize_exp sub_exp ~prefix in
    Bil.UnOp (unop, new_sub_exp) 
  | Bil.BinOp (binop, sub_exp_1, sub_exp_2) ->
    let new_sub_exp_1 = linearize_exp sub_exp_1 ~prefix in
    let new_sub_exp_2 = linearize_exp sub_exp_2 ~prefix in
    Bil.BinOp (binop, new_sub_exp_1, new_sub_exp_2)
  | Bil.Var v ->
    let new_var = linearize v ~prefix in
    Bil.Var new_var
  | Bil.Int _ -> exp
  | Bil.Cast (cast, i, sub_exp) ->
    let new_sub_exp = linearize_exp sub_exp ~prefix in
    Bil.Cast (cast, i, new_sub_exp)
  | Bil.Let (var, sub_exp_1, sub_exp_2) ->
    let new_sub_exp_1 = linearize_exp sub_exp_1 ~prefix in
    let new_sub_exp_2 = linearize_exp sub_exp_2 ~prefix in
    Bil.Let (var, new_sub_exp_1, new_sub_exp_2)
  | Bil.Ite (sub_exp_1, sub_exp_2, sub_exp_3) ->
    let new_sub_exp_1 = linearize_exp sub_exp_1 ~prefix in
    let new_sub_exp_2 = linearize_exp sub_exp_2 ~prefix in
    let new_sub_exp_3 = linearize_exp sub_exp_3 ~prefix in
    Bil.Ite (new_sub_exp_1, new_sub_exp_2, new_sub_exp_3)
  | Bil.Extract (i, j, sub_exp) ->
    let new_sub_exp = linearize_exp sub_exp ~prefix in
    Bil.Extract (i, j, new_sub_exp) 
  | Bil.Concat (sub_exp_1, sub_exp_2) ->
    let new_sub_exp_1 = linearize_exp sub_exp_1 ~prefix in
    let new_sub_exp_2 = linearize_exp sub_exp_2 ~prefix in
    Bil.Concat (new_sub_exp_1, new_sub_exp_2)
  | Bil.Unknown (_, _) -> exp

let linearize_phi ~(prefix : string) (phi : Phi.t) : Phi.t KB.t =
  ignore prefix;
  ignore phi;
  Kb_error.(fail @@ Not_implemented "Linear_ssa.linearize_phi: unimplemented")

let linearize_def ~(prefix : string) (def : Def.t) : Def.t KB.t =
  let lhs = Def.lhs def in
  let new_lhs = linearize lhs ~prefix in
  let new_def = Def.with_lhs def new_lhs in
  let rhs = Def.rhs new_def in
  let new_rhs = linearize_exp rhs ~prefix in
  KB.return @@ Def.with_rhs new_def new_rhs

let linearize_jmp ~(prefix : string) (jmp : Jmp.t) : Jmp.t KB.t =
  KB.return @@ Jmp.map_exp jmp ~f:(linearize_exp ~prefix)

let linearize_blk (blk : Blk.t) : Blk.t KB.t =
  let prefix = prefix_from blk in
  let* phis =
    Term.enum phi_t blk |> Seq.to_list |>
    KB.List.map ~f:(linearize_phi ~prefix) in
  let* defs =
    Term.enum def_t blk |> Seq.to_list |>
    KB.List.map ~f:(linearize_def ~prefix) in
  let+ jmps =
    Term.enum jmp_t blk |> Seq.to_list |>
    KB.List.map ~f:(linearize_jmp ~prefix) in
  Blk.create ~phis ~defs ~jmps ~tid:(Term.tid blk) ()
  
let transform (sub : Sub.t) : Blk.t list KB.t =
  Term.enum blk_t sub |> Seq.to_list |> KB.List.map ~f:linearize_blk
