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
open Monads.Std

module Naming = Substituter.Naming

let prefix_of_tid (tid : tid) : string =
  let tid_str = Tid.to_string tid in
  String.drop_prefix tid_str 1

(* Use the tid of the blk as the prefix, dropping the '%' at
   the beginning. *)
let prefix_from (blk : blk term) : string =
  prefix_of_tid @@ Term.tid blk

let linearize ~(prefix : string) (var : var) : var =
  let name = Var.name var in
  let typ = Var.typ var in
  let new_name = prefix ^ "_" ^ name in
  let escaped_name =
    String.substr_replace_all new_name ~pattern:"." ~with_:"_" in
  Var.create escaped_name typ

(* The prefix consists of an underscore (1 char), followed by the
   tid string (8 chars), ending with another underscore (1 char). *)
let prefix_len = 10

let orig_name (name : string) : string option =
  let (let*) x f = Option.bind x ~f in
  let name = String.drop_prefix name prefix_len in
  let name, is_reg =
    Naming.unmark_reg_name name |>
    Option.value_map ~default:(name, false) ~f:(fun name ->
        name, true) in
  let* name = match String.split name ~on:'_' with
    | [] -> Some name
    | [name] when not @@ String.is_empty name -> Some name
    | l ->
      let* l = List.drop_last l in
      Some (String.concat l ~sep:"_")
  in
  if is_reg then Some (Naming.mark_reg_name name) else Some name

let same (a : var) (b : var) : bool =
  let a = Var.name a and b = Var.name b in
  String.equal a b ||
  match (orig_name a), (orig_name b) with
  | Some a, Some b -> String.equal a b
  | _ ->  false

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

    let empty = {
      vars = Var.Set.empty;
      prefix = "";
    }

    let add_var v env = {env with vars = Set.add env.vars v}
    let with_prefix prefix env = {env with prefix}

  end

  include Monad.State.T1(Env)(Monad.Ident)
  include Monad.State.Make(Env)(Monad.Ident)

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
  let* defs = go def_t blk ~f:linearize_def in
  let+ jmps = go jmp_t blk ~f:linearize_jmp in
  (* We will deliberately remove phi nodes since they are subsumed by
     congruence between linear SSA vars. *)
  Blk.create ~phis:[] ~defs ~jmps ~tid:(Term.tid blk) ()

(* Produce a linear_ssa-ified live variable map. Each variable
   get the prefix from it's block.

   Also, expand the phi nodes to new defs at the corresponding
   edges in the CFG.
*)
let compute_liveness_and_expand_phis
    (hvars : Higher_var.t list)
    (sub : sub term) : (blk term list * Data.ins_outs Tid.Map.t) KB.t =
  let open KB.Let in
  let liveness = Live.compute sub in
  let blks = Term.enum blk_t sub |> Seq.to_list in
  (* Map each block to a list of pseudo definitions according to the
     phi nodes we discovered. *)
  let phi_map : (var * exp) list Tid.Map.t =
    List.fold blks ~init:Tid.Map.empty ~f:(fun init blk ->
        Term.enum phi_t blk |> Seq.fold ~init ~f:(fun init phi ->
            let lhs = Phi.lhs phi in
            Phi.values phi |> Seq.fold ~init ~f:(fun m (tid, e) ->
                Map.add_multi m ~key:tid ~data:(lhs, e)))) in
  (* Insert these pseudo-definitions. *)
  let* blks =
    KB.List.map blks ~f:(fun blk ->
        match Map.find phi_map @@ Term.tid blk with
        | None -> KB.return blk
        | Some defs ->
          let builder = Blk.Builder.init blk
              ~same_tid:true
              ~copy_phis:true
              ~copy_defs:true
              ~copy_jmps:true in
          let* () =
            KB.List.iter defs ~f:(fun (lhs, e) ->
                let+ tid = Theory.Label.fresh in
                let def = Def.create ~tid lhs e in
                Blk.Builder.add_def builder def) in
          let blk = Blk.Builder.result builder in
          KB.return blk) in
  (* Get the linearized ins and outs. *)
  let ins_outs_map = List.map blks ~f:(fun blk ->
      let tid = Term.tid blk in
      let outs = Live.outs liveness tid in
      let outs =
        (* Implicit exit blocks will have the `at-exit` finalizers,
           if they exist. *)
        if Bir_helpers.is_implicit_exit blk then
          (* Get all of the finalizers, which are assignments to
             preassigned registers. *)
          let vars =
            Term.enum def_t blk |> Seq.map ~f:Def.lhs |>
            Seq.filter ~f:(fun v -> Option.is_some @@ Naming.unmark_reg v) in
          (* Compare the higher var storage information with the
             existing var. We need the SSA'd name of this var. *)
          let same reg v =
            String.equal (Naming.mark_reg_name reg) @@
            Var.name @@ Var.base v in
          (* For each higher var we intended to finalize, add them
             to the set of live outs for this block. *)
          List.fold hvars ~init:outs ~f:(fun outs -> function
              | {value = Registers ({at_exit = Some reg; _}); _} -> begin
                  match Seq.find vars ~f:(same reg) with
                  | Some v -> Set.add outs v
                  | None -> outs
                end
              | _ -> outs)
        else outs in
      let ins = Live.ins liveness tid in
      let prefix = prefix_from blk in
      let outs = Var.Set.map outs ~f:(fun var -> linearize ~prefix var) in
      let ins = Var.Set.map ins ~f:(fun var -> linearize ~prefix var) in
      let ins_outs : Data.ins_outs = {ins; outs} in
      tid, ins_outs) in
  KB.return (blks, Tid.Map.of_alist_exn ins_outs_map)

let all_ins_outs_vars (ins_outs : Data.ins_outs Tid.Map.t) : Var.Set.t =
  Var.Set.union_list @@
  List.map ~f:(fun Data.{ins; outs} -> Var.Set.union ins outs) @@
  Tid.Map.data ins_outs

let transform
    ?(patch : Data.Patch.t option = None)
    (hvars : Higher_var.t list)
    (sub : sub term) : blk term list KB.t =
  let open KB.Let in
  let* blks, ins_outs_map = compute_liveness_and_expand_phis hvars sub in
  let blks, Linear.Env.{vars; _} =
    Linear.(run (List.map blks ~f:linearize_blk) Env.empty) in
  (* Add in live variables that persist across blocks that don't use them *)
  let vars = Var.Set.union vars (all_ins_outs_vars ins_outs_map) in
  let* () = match patch with
    | None -> KB.return ()
    | Some patch ->
      let* () = Data.Patch.set_ins_outs_map patch ins_outs_map in
      let cong v1 v2 = Var.(v1 <> v2) && congruent v1 v2 in
      Var.Set.to_list vars |> KB.List.iter ~f:(fun v1 ->
          let vars = Set.filter vars ~f:(cong v1) in
          Set.to_list vars |> KB.List.iter ~f:(fun v2 ->
              Data.Patch.add_congruence patch (v1, v2))) in
  KB.return blks
