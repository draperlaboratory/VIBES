open Core
open Bap.Std
open Monads.Std
open Bap_core_theory

module T = Theory
module Hvar = Vibes_higher_vars_lib.Higher_var
module Naming = Vibes_higher_vars_lib.Substituter.Naming
module Bir_helpers = Vibes_bir_lib.Helpers

module Linear = struct

  module Env = struct

    type t = {
      vars : Var.Set.t;
      prefix : Utils.prefix option;
    }

    let empty = {
      vars = Var.Set.empty;
      prefix = None;
    }

    let add_var v env = {env with vars = Set.add env.vars v}
    let with_prefix prefix env = {env with prefix}

  end

  include Monad.State.T1(Env)(Monad.Ident)
  include Monad.State.Make(Env)(Monad.Ident)

  let linearize_var (v : var) : var t =
    gets @@ fun {prefix; _} -> match prefix with
    | Some prefix -> Utils.linearize v ~prefix
    | None -> v

  let rec linearize_exp (exp : exp) : exp t = match exp with
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
      let+ () = update @@ Env.add_var new_var in
      Bil.Var new_var
    | Bil.Int _ -> !!exp
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
    | Bil.Unknown (_, _) -> !!exp

  let linearize_def (def : def term) : def term t =
    let lhs = Def.lhs def in
    let* new_lhs = linearize_var lhs in
    let* () = update @@ Env.add_var new_lhs in
    let new_def = Def.with_lhs def new_lhs in
    let rhs = Def.rhs new_def in
    let+ new_rhs = linearize_exp rhs in
    Def.with_rhs new_def new_rhs

  let linearize_jmp (jmp : jmp term) : jmp term t =
    let* cond = linearize_exp @@ Jmp.cond jmp in
    let of_label = function
      | Indirect e ->
        let+ e = linearize_exp e in
        Indirect e
      | lbl -> !!lbl in
    let of_kind = function
      | Call call ->
        let* call = match Call.return call with
          | None -> !!call
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
      | Int _ as k -> !!k in
    let+ kind = of_kind @@ Jmp.kind jmp in
    let tid = Term.tid jmp in
    Jmp.create ~tid ~cond kind

  let linearize_subterms
      (cls : ('a, 'b) cls)
      (t : 'a term)
      ~(f : 'b term -> 'b term t) : 'b term list t =
    Term.enum cls t |> Bap.Std.Seq.to_list |> List.map ~f

  let linearize_blk (blk : blk term) : blk term t =
    let tid = Term.tid blk in
    let prefix = Utils.prefix_of_tid tid in
    let* () = update @@ Env.with_prefix @@ Some prefix in
    let* defs = linearize_subterms def_t blk ~f:linearize_def in
    let+ jmps = linearize_subterms jmp_t blk ~f:linearize_jmp in
    (* We will deliberately remove phi nodes since they are
       subsumed by congruence between vars in linear SSA form. *)
    Blk.create ~phis:[] ~defs ~jmps ~tid ()

  let linearize_sub (sub : sub term) : sub term t =
    let tid = Term.tid sub in
    let name = Sub.name sub in
    let+ blks = linearize_subterms blk_t sub ~f:linearize_blk in
    let args = Term.enum arg_t sub |> Bap.Std.Seq.to_list in
    Sub.create ~args ~blks ~tid ~name ()

end

open KB.Syntax

(* Map each block to a list of pseudo definitions according to the
   phi nodes we discovered. *)
let collect_phi_defs (blks : blk term seq) : (var * exp) list Tid.Map.t =
  Seq.fold blks ~init:Tid.Map.empty ~f:(fun init blk ->
      Term.enum phi_t blk |> Seq.fold ~init ~f:(fun init phi ->
          let lhs = Phi.lhs phi in
          Phi.values phi |> Seq.fold ~init ~f:(fun m (tid, e) ->
              Map.add_multi m ~key:tid ~data:(lhs, e))))

(* Expand the phi nodes to new defs at the corresponding
   edges in the CFG. *)
let expand_phis
    (sub : sub term)
    (blks : blk term seq) : sub term KB.t =
  let phi_map = collect_phi_defs blks in
  Term.KB.map blk_t sub ~f:(fun blk ->
      match Map.find phi_map @@ Term.tid blk with
      | None -> !!blk
      | Some defs ->
        let+ defs = KB.List.map defs ~f:(fun (lhs, rhs) ->
            let+ tid = T.Label.fresh in
            Def.create ~tid lhs rhs) in
        List.fold defs ~init:blk ~f:(Term.append def_t))

let add_finalizers_to_outs
    (hvars : Hvar.t list)
    (blk : blk term)
    (outs : Var.Set.t) : Var.Set.t =
  (* Get all of the finalizers, which are assignments to
     preassigned registers. *)
  let vars =
    Term.enum def_t blk |> Seq.map ~f:Def.lhs |>
    Seq.filter ~f:(Fn.compose Option.is_some Naming.unmark_reg) in
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

(* Get the linearized ins and outs. *)
let construct_ins_outs_map
    (hvars : Hvar.t list)
    (blks : blk term seq)
    (liveness : Live.t) : Types.ins_outs_map =
  Seq.fold blks ~init:Tid.Map.empty ~f:(fun ins_outs_map blk ->
      let tid = Term.tid blk in
      let outs = Live.outs liveness tid in
      let outs =
        (* Implicit exit blocks will have the `at-exit` finalizers,
           if they exist. *)
        if not @@ Bir_helpers.no_jmps blk then outs
        else add_finalizers_to_outs hvars blk outs in
      let ins = Live.ins liveness tid in
      let prefix = Utils.prefix_of_tid tid in
      let outs = Var.Set.map outs ~f:(Utils.linearize ~prefix) in
      let ins = Var.Set.map ins ~f:(Utils.linearize ~prefix) in
      Map.set ins_outs_map ~key:tid ~data:Types.{ins; outs})

let compute_liveness_and_expand_phis
    (hvars : Hvar.t list)
    (sub : sub term) : (sub term * Types.ins_outs_map) KB.t =
  let liveness = Live.compute sub in
  let blks = Term.enum blk_t sub in
  let+ sub = expand_phis sub blks in
  sub, construct_ins_outs_map hvars blks liveness

(* Add in live variables that persist across blocks that don't
   use them. *)
let all_ins_outs_vars
    (vars : Var.Set.t)
    (ins_outs_map : Types.ins_outs_map) : Var.Set.t =
  Var.Set.union vars @@ Var.Set.union_list @@
  List.map ~f:(fun Types.{ins; outs} ->
      Var.Set.union ins outs) @@
  Tid.Map.data ins_outs_map

(* Produce a relation between congruent variables. *)
let collect_congruences
    (vars : Var.Set.t)
    (ins_outs_map : Types.ins_outs_map) : var Var.Map.t =
  let vars = all_ins_outs_vars vars ins_outs_map in
  let cong x y = Var.(x <> y) && Utils.congruent x y in
  Set.fold vars ~init:Var.Map.empty ~f:(fun init x ->
      Set.fold vars ~init ~f:(fun congruences y ->
          if not @@ cong x y then congruences
          else Map.set congruences ~key:x ~data:y))

let transform
    (hvars : Hvar.t list)
    (sub : sub term) : Types.t KB.t =
  let module Env = Linear.Env in
  let* sub = if Sub.is_ssa sub then !!sub else Sub.KB.ssa sub in
  let+ sub, ins_outs_map = compute_liveness_and_expand_phis hvars sub in
  let sub, env = Monad.State.run (Linear.linearize_sub sub) Env.empty in
  let congruences = collect_congruences env.Env.vars ins_outs_map in
  Types.{sub; ins_outs_map; congruences}
