(** Implements {!Linear_ssa}. *)

open Core_kernel
open Bap.Std

let congruent (var_1 : Var.t) (var_2 : Var.t) : bool =
  let name_1 = Var.name var_1 in
  let name_1 = String.drop_prefix name_1 10 in
  let name_2 = Var.name var_2 in
  let name_2 = String.drop_prefix name_2 10 in
  String.equal name_1 name_2

let prefix_from (blk : Blk.t) : string =
  let tid = Term.tid blk in
  let tid_str = Tid.to_string tid in
  String.drop_prefix tid_str 1

let linearize ~prefix:(prefix : string) (var : Var.t) : Var.t =
  let name = Var.name var in
  let typ = Var.typ var in
  let new_name = prefix ^ "_" ^ name in
  let escaped_name =
    String.substr_replace_all new_name ~pattern:"." ~with_:"_"
  in
  Var.create escaped_name typ

let rec linearize_exp ~prefix:(prefix : string) (exp : Bil.exp) : Bil.exp =
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

let linearize_def ~prefix:(prefix : string) (def : Def.t) : Def.t =
  let lhs = Def.lhs def in
  let new_lhs = linearize lhs ~prefix in
  let new_def = Def.with_lhs def new_lhs in
  let rhs = Def.rhs new_def in
  let new_rhs = linearize_exp rhs ~prefix in
  Def.with_rhs new_def new_rhs

let linearize_jmp ~prefix:(prefix : string) (jmp : Jmp.t) : Jmp.t =
  Jmp.map_exp jmp ~f:(fun exp -> linearize_exp exp ~prefix)

let linearize_blk (blk : Blk.t) : Blk.t =
  let prefix = prefix_from blk in
  let blk' = Term.map def_t blk ~f:(fun def -> linearize_def def ~prefix) in
  Term.map jmp_t blk' ~f:(fun jmp -> linearize_jmp jmp ~prefix)

let transform (sub : Sub.t) : Sub.t =
  Term.map blk_t sub ~f:(fun blk -> linearize_blk blk)
