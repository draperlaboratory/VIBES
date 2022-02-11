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
open Bap_knowledge
open Bap_core_theory
open Monads.Std

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

module Linear = struct

  module Env = struct

    type t = {
      vars : Var.Set.t;
      prefix : string;
    }

    let add_var v env = {env with vars = Set.add env.vars v}
    let with_prefix prefix env = {env with prefix}

  end

  include Monad.State.T1(Env)(Knowledge)
  include Monad.State.Make(Env)(Knowledge)

end

open Linear.Let

type 'a linear = 'a Linear.t

let linearize_var (v : var) : var linear =
  Linear.gets @@ fun {prefix; _} -> linearize v ~prefix

let rec linearize_exp (exp : exp) : exp linear = match exp with
  | Bil.Load (sub_exp_1, sub_exp_2, endian, size) ->
    let* new_sub_exp_1 = linearize_exp sub_exp_1 in
    let+ new_sub_exp_2 = linearize_exp sub_exp_2 in
    Bil.Load (new_sub_exp_1, new_sub_exp_2, endian, size) 
  | Bil.Store (sub_exp_1, sub_exp_2, sub_exp_3, endian, size) ->
    let* new_sub_exp_1 = linearize_exp sub_exp_1 in
    let* new_sub_exp_2 = linearize_exp sub_exp_2 in
    let+ new_sub_exp_3 = linearize_exp sub_exp_3 in
    Bil.Store (new_sub_exp_1, new_sub_exp_2, new_sub_exp_3, endian, size)
  | Bil.UnOp (unop, sub_exp) -> 
    let+ new_sub_exp = linearize_exp sub_exp in
    Bil.UnOp (unop, new_sub_exp) 
  | Bil.BinOp (binop, sub_exp_1, sub_exp_2) ->
    let* new_sub_exp_1 = linearize_exp sub_exp_1 in
    let+ new_sub_exp_2 = linearize_exp sub_exp_2 in
    Bil.BinOp (binop, new_sub_exp_1, new_sub_exp_2)
  | Bil.Var v ->
    let* new_var = linearize_var v in
    let+ () = Linear.(update @@ Env.add_var new_var) in
    Bil.Var new_var
  | Bil.Int _ -> Linear.return exp
  | Bil.Cast (cast, i, sub_exp) ->
    let+ new_sub_exp = linearize_exp sub_exp in
    Bil.Cast (cast, i, new_sub_exp)
  | Bil.Let (var, sub_exp_1, sub_exp_2) ->
    let* new_sub_exp_1 = linearize_exp sub_exp_1 in
    let+ new_sub_exp_2 = linearize_exp sub_exp_2 in
    Bil.Let (var, new_sub_exp_1, new_sub_exp_2)
  | Bil.Ite (sub_exp_1, sub_exp_2, sub_exp_3) ->
    let* new_sub_exp_1 = linearize_exp sub_exp_1 in
    let* new_sub_exp_2 = linearize_exp sub_exp_2 in
    let+ new_sub_exp_3 = linearize_exp sub_exp_3 in
    Bil.Ite (new_sub_exp_1, new_sub_exp_2, new_sub_exp_3)
  | Bil.Extract (i, j, sub_exp) ->
    let+ new_sub_exp = linearize_exp sub_exp in
    Bil.Extract (i, j, new_sub_exp) 
  | Bil.Concat (sub_exp_1, sub_exp_2) ->
    let* new_sub_exp_1 = linearize_exp sub_exp_1 in
    let+ new_sub_exp_2 = linearize_exp sub_exp_2 in
    Bil.Concat (new_sub_exp_1, new_sub_exp_2)
  | Bil.Unknown (_, _) -> Linear.return exp

let linearize_phi (phi : phi term) : phi term linear =
  let lhs = Phi.lhs phi in
  let* new_lhs = linearize_var lhs in
  let+ new_values =
    Phi.values phi |> Seq.to_list |>
    Linear.List.map ~f:(fun (tid, e) ->
        let+ e = linearize_exp e in
        tid, e) in
  Phi.of_list ~tid:(Term.tid phi) new_lhs new_values

let linearize_def (def : def term) : def term linear =
  let lhs = Def.lhs def in
  let* new_lhs = linearize_var lhs in
  let* () = Linear.(update @@ Env.add_var new_lhs) in
  let new_def = Def.with_lhs def new_lhs in
  let rhs = Def.rhs new_def in
  let+ new_rhs = linearize_exp rhs in
  Def.with_rhs new_def new_rhs

let linearize_jmp (jmp : jmp term) : jmp term linear =
  let* cond = linearize_exp @@ Jmp.cond jmp in
  let of_label = function
    | Indirect e ->
      let+ e = linearize_exp e in
      Indirect e
    | lbl -> Linear.return lbl in
  let of_kind = function
    | Call call ->
      let* call = match Call.return call with
        | None -> Linear.return call
        | Some lbl ->
          let+ lbl = of_label lbl in
          Call.with_return call lbl in
      let+ tgt = of_label @@ Call.target call in
      Call (Call.with_target call tgt)
    | Goto lbl ->
      let+ lbl = of_label lbl in
      Goto lbl
    | Ret lbl ->
      let+ lbl = of_label lbl in
      Ret lbl
    | Int _ as k -> Linear.return k in
  let+ kind = of_kind @@ Jmp.kind jmp in
  let tid = Term.tid jmp in
  Jmp.create ~tid ~cond kind

let go (cls : ('a, 'b) cls) (t : 'a term)
    ~(f : 'b term -> 'b term linear) : 'b term list linear =
  Term.enum cls t |> Seq.to_list |> Linear.List.map ~f

let linearize_blk (blk : blk term) : blk term linear =
  let prefix = prefix_from blk in
  let* () = Linear.(update @@ Env.with_prefix prefix) in
  let* phis = go phi_t blk ~f:linearize_phi in
  let* defs = go def_t blk ~f:linearize_def in
  let+ jmps = go jmp_t blk ~f:linearize_jmp in
  Blk.create ~phis ~defs ~jmps ~tid:(Term.tid blk) ()

let transform (sub : sub term) : blk term list KB.t =
  let open KB.Let in
  let* blks, {vars; _} = 
    Linear.Env.{prefix = ""; vars = Var.Set.empty} |>
    Linear.run (go blk_t sub ~f:linearize_blk) in
  let+ () =
    let cong v1 v2 = Var.(v1 <> v2) && congruent v1 v2 in
    Var.Set.to_list vars |>
    KB.List.iter ~f:(fun v1 ->
        let vars = Set.filter vars ~f:(cong v1) |> Set.to_list in
        KB.List.iter vars ~f:(fun v2 ->
            let* cong = KB.Object.create Congruence.cls in
            KB.provide Congruence.slot cong @@ Some (v1, v2))) in
  blks
