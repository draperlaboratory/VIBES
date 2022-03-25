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

(* Implements {!Bir_opt}. *)

open !Core_kernel
open Bap.Std
open Bap_core_theory
open Graphlib.Std
open KB.Let

module Arm = Arm_selector
(* module Subst = Substituter *)
module Hvar = Higher_var
module Err = Kb_error

type t = {
  ir : blk term list;
  cfg : Graphs.Tid.t;
  exclude_regs : String.Set.t;
  argument_tids : Tid.Set.t;
}

(* Helper functions *)
module Helper = struct

  (* Create a dummy subroutine from the blocks. *)
  let create_sub (blks : blk term list) : sub term KB.t =
    let+ tid = Theory.Label.fresh in
    Sub.create ~name:"dummy-wrapper" ~blks ~tid ()

  (* Find the exit nodes of the patch code. They are classified as follows:

     - no jmps:
       this is implicitly an exit block since it has no successors
       in the CFG.

     - goto <indirect>:
       this block is explicitly jumping out of the program
       to somewhere else in the binary.

     - call <x> with noreturn:
       this block must implicitly return to an exit block.

     - call <x> with return <indirect>:
       this block will not be returning to the patch code.
  *)
  let exit_blks (blks : blk term list) : blk term list KB.t =
    match blks with
    | [] -> Kb_error.fail @@ Other
        "Bir_passes.Helper.exit_blks: got an empty list of blks"
    | [_] -> KB.return blks
    | _ ->
      let+ sub = create_sub blks in
      let cfg = Sub.to_cfg sub in
      Graphs.Ir.nodes cfg |> Seq.to_list |>
      List.filter_map ~f:(fun node ->
          let blk = Graphs.Ir.Node.label node in
          if Graphs.Ir.Node.degree node cfg ~dir:`Out = 0
          then Some blk
          else
            let jmps = Term.enum jmp_t blk in
            if Seq.is_empty jmps then Some blk
            else if Seq.exists jmps ~f:(fun jmp ->
                match Jmp.kind jmp with
                | Call call -> begin
                    match Call.return call with
                    | None | Some (Indirect _) -> true
                    | _ -> false
                  end
                | _ -> false)
            then Some blk
            else None)

  (* Returns true if the block contains a call to a subroutine. *)
  let has_call (blk : blk term) : bool =
    Term.enum jmp_t blk |> Seq.exists ~f:(fun jmp ->
        match Jmp.kind jmp with
        | Call _ -> true
        | _ -> false)

  (* Returns true if there are calls in the IR. *)
  let call_blks (blks : blk term list) : tid list =
    List.filter_map blks ~f:(fun blk ->
        if has_call blk then Some (Term.tid blk) else None)

  (* Returns true if the jmp is unconditional. *)
  let is_unconditional (jmp : jmp term) : bool =
    match Jmp.cond jmp with
    | Int w -> Word.(w = b1)
    | _ -> false

end

(* BIR optimizations. All of these passes assume that the blocks are ordered
   according to `Shape.reorder_blks`, and that the program is NOT YET in SSA
   form. *)
module Opt = struct

  type t = blk term list -> blk term list KB.t

  module Contract = struct

    (* Edge contraction: https://en.wikipedia.org/wiki/Edge_contraction *)
    let go : t = fun blks ->
      let module G = Graphs.Tid in
      (* Since we assume the correct ordering of the blocks, the entry tid
         should be the first in the list. *)
      let* entry_tid = match blks with
        | [] -> Kb_error.fail @@ Other
            "Bir_passes.Contract: no entry block found"
        | blk :: _ -> KB.return @@ Term.tid blk in
      let rec loop blks =
        (* Collect all blocks that contain only a single unconditional Goto.
           In other words, these blocks are just "middlemen" and can thus be
           "cut out". Predecessors of these blocks can then have their
           control flow redirected to the successors of these "middlemen". *)
        let singles =
          List.filter_map blks ~f:(fun blk ->
              let tid = Term.tid blk in
              (* Note that the entry block can never be a candidate for
                 contraction. *)
              if Tid.(tid <> entry_tid)
              && Seq.is_empty @@ Term.enum def_t blk then
                let jmps = Term.enum jmp_t blk |> Seq.to_list in
                match jmps with
                | [jmp] when Helper.is_unconditional jmp -> begin
                    match Jmp.kind jmp with
                    | Goto lbl -> Some (tid, lbl)
                    | _ -> None
                  end
                | _ -> None
              else None) |>
          Tid.Table.of_alist_exn in
        (* If `tid` is a "single", and it is part of an arbitrarily long chain
           of "singles", then we can try to chase down the final destination.
           Since we may encounter a cycle, we carry around the set of visited
           nodes. *)
        let rec find_single_dst ?(visited = Tid.Set.empty) tid =
          if Set.mem visited tid then None
          else
            let visited = Set.add visited tid in
            Tid.Table.find singles tid |> Option.bind ~f:(function
                | Indirect _ as ind -> Some ind
                | Direct tid' as dir -> match find_single_dst tid' ~visited with
                  | Some _ as next -> next
                  | None -> Some dir) in
        let contracted, blks, changed =
          let init = Tid.Set.empty, [], false in
          List.fold blks ~init ~f:(fun (contracted, blks, changed) blk ->
              let tid = Term.tid blk in
              (* Ignore "singles" that were part of a contraction on
                 this iteration. We will keep them in the IR until they
                 are no longer reachable. *)
              if Set.mem contracted tid
              then contracted, blk :: blks, changed
              else
                let contracted = ref contracted in
                let changed = ref changed in
                let blk = Term.map jmp_t blk ~f:(fun jmp ->
                    match Jmp.kind jmp with
                    | Goto (Direct tid') when Tid.(tid <> tid') -> begin
                        match find_single_dst tid' with
                        | Some (Direct tid'') when Tid.(tid' <> tid'') ->
                          contracted := Set.add !contracted tid';
                          changed := true;
                          Jmp.with_kind jmp @@ Goto (Direct tid'')
                        | Some (Indirect _ as ind) ->
                          contracted := Set.add !contracted tid';
                          changed := true;
                          Jmp.with_kind jmp @@ Goto ind
                        | None | Some (Direct _) -> jmp
                      end
                    | _ -> jmp) in
                !contracted, blk :: blks, !changed) in
        (* Repeat until we reach a fixed point. *)
        let blks = List.rev blks in
        if changed then
          (* Remove unreachable nodes before iterating again. *)
          let* sub = Helper.create_sub blks in
          let cfg = Sub.to_graph sub in
          loop @@ List.filter blks ~f:(fun blk ->
              let tid = Term.tid blk in
              if Set.mem contracted tid then
                let preds =
                  G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
                not @@ Tid.Set.(is_empty @@ remove preds G.start)
              else true)
        else KB.return blks in
      loop blks

  end

  module Simpl = struct

    let simplify_exp : exp -> exp = Exp.simpl ~ignore:Eff.[read]

    let simplify_blk (blk : blk term) : blk term =
      Term.map def_t blk ~f:(fun def ->
          Def.with_rhs def @@ simplify_exp @@ Def.rhs def) |>
      Term.map jmp_t ~f:(fun jmp ->
          Jmp.with_cond jmp @@ simplify_exp @@ Jmp.cond jmp)

    (* Simplify expressions in blocks. *)
    let go : t = fun blks -> KB.return @@ List.map blks ~f:simplify_blk

  end

  module Merge = struct

    (* If two blocks have a single, unconditional edge in between them,
       then they can be merged together. This requires a particular
       ordering for the blocks, since blocks that get merged into their
       predecessors will get deleted. Therefore, they must be visited
       afterwards in the ordering. *)
    let go : t = fun blks ->
      let module G = Graphs.Tid in
      let rec loop blks =
        let* sub = Helper.create_sub blks in
        with_cfg (Sub.to_graph sub) blks
      and with_cfg cfg blks =
        (* Map tids to blocks. *)
        let blk_table = Tid.Table.create () in
        List.iter blks ~f:(fun blk ->
            Tid.Table.set blk_table ~key:(Term.tid blk) ~data:blk);
        (* Get the implicit exit block. *)
        let exit_tid = List.find_map blks ~f:(fun blk ->
            if Seq.is_empty @@ Term.enum jmp_t blk
            then Some (Term.tid blk)
            else None) in
        (* We can merge with a successor block if we are its only predecessor.
           Note that the pseudo-start node will be a predecessor of
           "unreachable" blocks. Furthermore, the implicit exit block cannot
           be merged with. *)
        let can_merge tid =
          not (Option.exists exit_tid ~f:(Tid.equal tid)) &&
          let preds =
            G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
          Set.(length @@ remove preds G.start) = 1 in
        let merged = ref Tid.Set.empty in
        let blks =
          List.filter_map blks ~f:(fun blk ->
              let tid = Term.tid blk in
              if Tid.Set.mem !merged tid then None
              else match Term.enum jmp_t blk |> Seq.to_list with
                | [jmp] when Helper.is_unconditional jmp -> begin
                    match Jmp.kind jmp with
                    | Goto (Direct tid') when Tid.(tid <> tid') ->
                      if can_merge tid' then
                        (* This must run before the SSA pass, so there should
                           not be any phi nodes in the program. *)
                        let blk' = Tid.Table.find_exn blk_table tid' in
                        let defs = Term.enum def_t blk |> Seq.to_list in
                        let defs' = Term.enum def_t blk' |> Seq.to_list in
                        let jmps' = Term.enum jmp_t blk' |> Seq.to_list in
                        let blk = Blk.create ()
                            ~tid ~defs:(defs @ defs') ~jmps:jmps' in
                        merged := Tid.Set.add !merged tid';
                        Some blk
                      else
                        (* An implicit fallthrough is possible here, but
                           we should defer that until after selection,
                           scheduling, and allocation. *)
                        Some blk
                    | Goto _ | Call _ | Ret _ | Int _ -> Some blk
                  end
                | _ -> Some blk) in
        (* If we changed anything, then recompute the CFG and repeat the
           optimization. We terminate when a fixed point is reached. *)
        if Tid.Set.is_empty !merged
        then KB.return blks
        else loop blks in
      loop blks

  end

  (* Applies all the optimizations in the list *)
  let apply_list (opts : t list) : t = fun init ->
    KB.List.fold opts ~init ~f:(fun ir opt -> opt ir)

  (* Applies all the optimizations we currently perform. *)
  let apply : t = apply_list [
      Simpl.go;
      Merge.go;
      Contract.go;
    ]

  (* This code is taken directly from BAP's optimization passes:

     https://github.com/BinaryAnalysisPlatform/bap/tree/master/plugins/optimization

     Since this is a plugin and not a library, it's not exposed for
     programmatic use.

     For our purposes, we will use an optimization level of 1, so
     only virtual variables will be touched.
  *)
  module Bap_opt = struct

    (* Optimization_data *)
    module O = struct
      type jmp_update = {
        cond : exp;
        kind : jmp_kind;
      }

      type update = Rhs of exp | Jmp of jmp_update

      type t = {
        deads   : Tid.Set.t;
        updates : update Tid.Map.t;
      }

      let updated_term = Value.Tag.register (module Unit)
          ~name:"vibes-updated-term"
          ~uuid:"98dce3a9-3831-452c-a7b7-4d26f23ad23e"

      let mark_updated t = Term.set_attr t updated_term ()
      let is_updated t = Option.is_some (Term.get_attr t updated_term)

      let drop_index = (object
        inherit Exp.mapper
        method! map_sym var = Var.base var
      end)#map_exp

      let updates_of_sub sub =
        let fold t cls init ~f = Seq.fold (Term.enum cls t) ~init ~f in
        let update_rhs updates d =
          let rhs = drop_index (Def.rhs d) in
          Map.set updates (Term.tid d) (Rhs rhs) in
        let update_jmp updates j =
          let j = Jmp.map_exp ~f:drop_index j in
          let data = Jmp {cond = Jmp.cond j; kind = Jmp.kind j} in
          Map.set updates (Term.tid j) data in
        let add_if add updates t =
          if is_updated t then add updates t else updates in
        fold sub blk_t Tid.Map.empty ~f:(fun updates b ->
            fold b def_t updates ~f:(add_if update_rhs) |>
            fold b jmp_t ~f:(add_if update_jmp))

      let create ~deads sub = {deads; updates = updates_of_sub sub}

      let (++) = Set.union

      let dead_jmps_of_blk b =
        Term.to_sequence jmp_t b |>
        Seq.fold ~init:(Set.empty (module Tid), false)
          ~f:(fun (deads, is_unreachable) jmp ->
              if is_unreachable
              then Set.add deads (Term.tid jmp), is_unreachable
              else match Jmp.cond jmp with
                | Bil.Int x when Word.(x = b1) -> deads, true
                | Bil.Int x when Word.(x = b0) ->
                  Set.add deads (Term.tid jmp), is_unreachable
                | _ -> deads, is_unreachable) |> fst

      let dead_jmps sub =
        Term.to_sequence blk_t sub |>
        Seq.fold ~init:(Set.empty (module Tid))
          ~f:(fun tids b -> tids ++ dead_jmps_of_blk b)

      let remove_dead_edges g dead_jmps =
        let module G = Graphs.Tid in
        Seq.fold (G.edges g)
          ~init:g ~f:(fun g edge ->
              if Set.mem dead_jmps (Graphs.Tid.Edge.label edge)
              then G.Edge.remove edge g
              else g)

      let dead_blks g =
        let module G = Graphs.Tid in
        fst @@
        Graphlib.depth_first_search (module G) g
          ~init:(Set.empty (module Tid), false)
          ~start_tree:(fun node (deads, _) ->
              deads, Tid.equal node G.start)
          ~enter_node:(fun _ node (deads, is_reachable) ->
              if is_reachable then deads, is_reachable
              else Set.add deads node, is_reachable)

      let find_unreachable sub t =
        let dead_jmps = dead_jmps sub in
        let dead_blks =
          remove_dead_edges (Sub.to_graph sub) dead_jmps |>
          dead_blks in
        {t with deads = t.deads ++ dead_jmps ++ dead_blks }

      let update_def updates d =
        match Map.find updates (Term.tid d) with
        | None -> d
        | Some (Rhs e) -> Def.with_rhs d e
        | _ -> assert false

      let update_jmp updates j =
        match Map.find updates (Term.tid j) with
        | None -> j
        | Some (Jmp {cond; kind}) ->
          let j = Jmp.with_cond j cond in
          Jmp.with_kind j kind
        | _ -> assert false

      let update sub {updates} =
        Term.map blk_t sub ~f:(fun b ->
            Term.map def_t b ~f:(update_def updates) |>
            Term.map jmp_t ~f:(update_jmp updates))

      let filter_map_alive deads cls ?(f=ident) x =
        Term.filter_map cls x ~f:(fun t ->
            if Set.mem deads (Term.tid t) then None
            else Some (f t))

      let remove_dead_code sub {deads} =
        let update_blk b =
          filter_map_alive deads def_t b |>
          filter_map_alive deads jmp_t in
        filter_map_alive deads blk_t sub ~f:update_blk

      let apply sub {deads; updates} =
        let update_blk b =
          filter_map_alive deads def_t b ~f:(update_def updates) |>
          filter_map_alive deads jmp_t ~f:(update_jmp updates) in
        filter_map_alive deads blk_t sub ~f:update_blk
    end

    let is_optimization_allowed var =
      Var.is_virtual var

    let def_use_collector = object
      inherit [Var.Set.t * Var.Set.t] Term.visitor

      method! enter_def t (defs,uses) =
        Set.add defs (Def.lhs t), Set.union uses (Def.free_vars t)

      method! enter_phi t (defs,uses) =
        Set.add defs (Phi.lhs t), Set.union uses (Phi.free_vars t)

      method! enter_jmp t (defs,uses) =
        defs, Set.union uses (Jmp.free_vars t)
    end

    let computed_def_use sub =
      def_use_collector#visit_sub sub (Var.Set.empty,Var.Set.empty)

    let compute_dead can_touch protected sub =
      let defs,uses = computed_def_use sub in
      let dead = Set.diff defs uses in
      let live v = not (Set.mem dead v) in
      Term.enum blk_t sub |>
      Seq.fold ~init:Tid.Set.empty ~f:(fun dead blk ->
          Term.enum ~rev:true def_t blk |>
          Seq.fold ~init:(protected,dead) ~f:(fun (protected,dead) def ->
              let v = Def.lhs def in
              if not (can_touch v) || live v
              then (protected,dead)
              else if Set.mem protected (Var.base v)
              then Set.remove protected (Var.base v), dead
              else protected, Set.add dead (Term.tid def)) |>
          snd)

    let is_alive dead t = not (Set.mem dead (Term.tid t))
    let live_phi dead blk = Term.filter phi_t ~f:(is_alive dead) blk

    let live_def can_touch dead blk =
      Term.filter def_t blk ~f:(fun d ->
          not (can_touch (Def.lhs d)) || is_alive dead d)

    let rec substitute vars exp =
      let substituter = object
        inherit Exp.mapper as super
        method! map_let var ~exp ~body =
          let exp = super#map_exp exp in
          let body = substitute (Map.remove vars var) body in
          Bil.let_ var exp body
        method! map_var v = match Map.find vars v with
          | None -> Bil.var v
          | Some e -> e
      end in
      substituter#map_exp exp |> Exp.fold_consts

    let equal_kinds j j' = compare_jmp_kind (Jmp.kind j) (Jmp.kind j') = 0

    let substitute sub vars =
      let def d =
        let rhs = substitute vars (Def.rhs d) in
        if Exp.(Def.rhs d <> rhs) then
          O.mark_updated (Def.with_rhs d rhs)
        else d in
      let jmp j =
        let j' = Jmp.map_exp j ~f:(substitute vars) in
        if Exp.(Jmp.cond j <> Jmp.cond j') || not (equal_kinds j j')
        then O.mark_updated j'
        else j in
      Term.map blk_t sub ~f:(Blk.map_elts ~def ~jmp)

    (* A simple constant propagation. Note, the input is required to
       be in SSA. *)
    let propagate_consts can_touch sub =
      Seq.fold (Term.enum blk_t sub) ~init:Var.Map.empty
        ~f:(fun vars b ->
            Seq.fold (Term.enum def_t b) ~init:vars
              ~f:(fun vars d ->
                  let v = Def.lhs d in
                  if can_touch v then
                    match Def.rhs d with
                    | Bil.Unknown _ | Bil.Int _ as exp ->
                      Map.set vars ~key:v ~data:exp
                    | _ -> vars
                  else vars)) |>
      substitute sub

    let clean can_touch dead sub =
      Term.map blk_t sub ~f:(fun b ->
          live_def can_touch dead b |> live_phi dead)

    let process_sub free can_touch sub =
      let rec loop dead s =
        let s = propagate_consts can_touch s in
        let dead' = compute_dead can_touch free s in
        let dead = Set.union dead dead' in
        if Set.is_empty dead' then s, dead
        else loop dead (clean can_touch dead' s) in
      let sub', dead = loop Tid.Set.empty (Sub.ssa sub) in
      O.create dead sub'

    let run sub =
      let free = Sub.free_vars sub in
      let can_touch = is_optimization_allowed in
      let data = process_sub free can_touch sub in
      let sub = O.update sub data in
      let data = O.find_unreachable sub data in
      O.remove_dead_code sub data

  end

end

(* Change the shape of the code for the instruction selector. *)
module Shape = struct

  (* Remove any unreachable blocks. *)
  let remove_unreachable
      (blks : blk term list)
      (entry_tid : tid) : blk term list KB.t =
    let module G = Graphs.Tid in
    let+ sub = Helper.create_sub blks in
    let cfg = Sub.to_graph sub in
    List.filter blks ~f:(fun blk ->
        let tid = Term.tid blk in
        Tid.(tid = entry_tid) ||
        let preds =
          G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
        not @@ Tid.Set.(is_empty @@ remove preds G.start))

  (* We need to insert explicit exit blocks when we encounter the following
     kinds of jmps:

     - call <x> with noreturn
     - call <x> with return <indirect>
     - no jmps at all

     With noreturn calls and no jmps at all, we can have them share the
     same exit block at the very end of the program.

     With return <indirect>, we need to make unique exit blocks for each
     indirect target.
  *)
  let adjust_exits (blks : blk term list) : blk term list KB.t =
    let* exit_tids =
      let+ exits = Helper.exit_blks blks in
      List.map exits ~f:Term.tid |> Tid.Set.of_list in
    (* This is the block that we may insert as a continuation
       for `call ... with noreturn`, as well as blocks with no jumps
       at all. Since such blocks have no particular successor in the
       CFG, this block can be shared between them. *)
    let extra = ref None in
    let make_extra () = match !extra with
      | Some blk -> KB.return @@ Term.tid blk
      | None ->
        let+ tid = Theory.Label.fresh in
        extra := Some (Blk.create ~tid ());
        tid in
    (* Returns to indirect targets, on the other hand, require us
       to make a unique block for each target. *)
    let extra_indirect = ref [] in
    (* Collect information about jmps that need to be rewrtitten. *)
    let* blks = KB.List.map blks ~f:(fun blk ->
        let tid = Term.tid blk in
        if Set.mem exit_tids tid then
          match Term.enum jmp_t blk |> Seq.to_list with
          | [] ->
            (* The block has no jumps, so it is implicitly an exit node.
               Make this explicitly jump to a new exit block at the very
               end of the program. *)
            let* jmp_tid = Theory.Label.fresh in
            let+ tid' = make_extra () in
            let jmp = Jmp.create_goto ~tid:jmp_tid (Direct tid') in
            let defs = Term.enum def_t blk |> Seq.to_list in
            Blk.create ~tid ~jmps:[jmp] ~defs ()
          | jmps ->
            let+ jmps = KB.List.map jmps ~f:(fun jmp ->
                match Jmp.kind jmp with
                | Call call -> begin
                    match Call.return call with
                    | Some (Direct _) -> KB.return jmp
                    | Some (Indirect _ as label) ->
                      (* We need a unique exit node that has an indirect
                         `goto label`. *)
                      let+ tid = Theory.Label.fresh in
                      let blk =
                        Blk.create ~tid ~jmps:[Jmp.create_goto label] () in
                      extra_indirect := blk :: !extra_indirect;
                      Jmp.(with_dst jmp @@ Some (resolved tid))
                    | None ->
                      (* Calls should not be noreturn. Since BAP didn't give
                         this any particular target to return to, we can
                         have it return to a common exit block. *)
                      let+ tid = make_extra () in
                      Jmp.(with_dst jmp @@ Some (resolved tid))
                  end
                | _ -> KB.return jmp) in
            let defs = Term.enum def_t blk |> Seq.to_list in
            Blk.create ~tid ~defs ~jmps ()
        else KB.return blk) in
    (* Append the generated blocks. *)
    let extra = Option.value_map !extra ~default:[] ~f:List.return in
    KB.return (blks @ !extra_indirect @ extra)

  (* Order the blocks according to a reverse postorder DFS traversal.
     This should minimize the number of extra jumps we need to insert. *)
  let reorder_blks (blks : blk term list) : blk term list KB.t =
    let+ sub = Helper.create_sub blks in
    let cfg = Sub.to_cfg sub in
    let blks =
      Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg |>
      Seq.to_list |> List.map ~f:Graphs.Ir.Node.label in
    (* The exit block with no jmps should be ordered at the very end of
       the program. *)
    match List.find blks ~f:(fun blk ->
        Seq.is_empty @@ Term.enum jmp_t blk) with
    | None -> blks
    | Some blk ->
      let tid = Term.tid blk in
      let blks = List.filter blks ~f:(fun blk' ->
          Tid.(tid <> Term.tid blk')) in
      blks @ [blk]

end

(* Handle storage classification in the presence of ABI information. *)
module ABI = struct

  (* Collect the args to the call, if any. *)
  let collect_args (blk : blk term) : var list option KB.t =
    Term.enum jmp_t blk |> KB.Seq.find_map ~f:(fun jmp ->
        match Jmp.alt jmp with
        | None -> KB.return None
        | Some dst -> match Jmp.resolve dst with
          | Second _ -> KB.return None
          | First tid ->
            let+ args = Core_c.collect_args tid in
            Some args)

  (* Collect the tids where the arguments to calls are defined. *)
  let collect_argument_tids (blks : blk term list) : Tid.Set.t KB.t =
    KB.List.fold blks ~init:Tid.Set.empty ~f:(fun acc blk ->
        let+ args = collect_args blk in
        Option.value_map args ~default:acc ~f:(fun args ->
            Term.enum def_t blk |> Seq.to_list |>
            List.fold_right ~init:(Var.Set.of_list args, acc)
              ~f:(fun def (remaining, acc) ->
                  let lhs = Def.lhs def in
                  if Var.Set.mem remaining lhs then (
                    Var.Set.remove remaining lhs,
                    Tid.Set.add acc @@ Term.tid def
                  ) else (remaining, acc)) |> snd))

  (* Create a fake memory assignment which is a signpost to the selector
     that the most recently assigned memory is a dependency (or "argument")
     of the function call. *)
  let insert_new_mems_at_callsites (tgt : Theory.target)
      (blks : blk term list) : (blk term list * Tid.Set.t) KB.t =
    let mem = Var.reify @@ Theory.Target.data tgt in
    let+ blks, tids = KB.List.fold blks ~init:([], Tid.Set.empty)
        ~f:(fun (blks, tids) blk ->
            if Helper.has_call blk then
              let+ tid = Theory.Label.fresh in
              let lhs = Var.create (Var.name mem ^ "_call") (Var.typ mem) in
              let def = Def.create ~tid lhs @@ Var mem in
              let blk = Term.append def_t blk def in
              blk :: blks, Tid.Set.add tids tid
            else KB.return (blk :: blks, tids)) in
    List.rev blks, tids

  (* We shouldn't do any spilling if there are higher vars that depend
     on SP-relative locations in memory. *)
  let check_hvars_for_existing_stack_locations
      (sp : var) (hvars : Hvar.t list) : unit KB.t =
    KB.List.iter hvars ~f:(fun hvar ->
        match Hvar.value hvar with
        | Hvar.(Storage {at_entry = Memory (Frame (reg, offset)); _}) ->
          if String.(reg <> Var.name sp) then KB.return ()
          else Err.(fail @@ Other (
              sprintf "Existing stack location [%s, %s] used by hvar %s"
                reg (Word.to_string offset) (Hvar.name hvar)))
        | _ -> KB.return ())

  (* Spill higher vars in caller-save registers if we are doing any calls
     in a multi-block patch, since the ABI says they may be clobbered. *)
  let spill_hvars_and_adjust_stack
      (blks : blk term list)
      ~(tgt : Theory.target)
      ~(sp_align : int)
      ~(hvars : Hvar.t list)
      ~(entry_blk : blk term)
    : (blk term list * Higher_var.t list * String.Set.t) KB.t =
    let calls = Helper.call_blks blks in
    if List.is_empty calls || List.length blks = 1
    then KB.return (blks, hvars, String.Set.empty)
    else
      (* Collect the liveness information. *)
      let* live =
        let+ sub = Helper.create_sub blks in
        Live.compute sub in
      (* Returns true if the variable is live after a call. *)
      let live_after_call v = List.exists calls ~f:(fun tid ->
          let out = Live.outs live tid in
          Set.exists out ~f:(fun v' -> String.(v = Var.name v'))) in
      let width = Theory.Target.bits tgt in
      let stride = Word.of_int ~width (width lsr 3) in
      (* Use predetermined stack locations. *)
      let caller_save =
        Theory.Target.regs tgt ~roles:Theory.Role.Register.[caller_saved] |>
        Set.to_list |> List.mapi ~f:(fun i v ->
            let idx = Word.of_int ~width i in
            let v = Var.reify v in
            let name = Var.name v in
            name, (Word.(stride * idx), v)) |>
        String.Map.of_alist_exn in
      let* sp =
        match Theory.Target.reg tgt Theory.Role.Register.stack_pointer with
        | Some v -> KB.return @@ Var.reify v
        | None -> Err.(fail @@ Other (
            sprintf "No stack pointer register for target %s\n%!"
              (Theory.Target.to_string tgt))) in
      (* Preserved and restored registers. *)
      let preserved = ref String.Set.empty in
      let restored = ref String.Set.empty in
      (* Spilled variables. *)
      let spilled = ref String.Set.empty in
      let spill ?(restore = false) name v offset hvar =
        let memory = Hvar.create_frame (Var.name sp) offset in
        let at_entry = Hvar.stored_in_memory memory in
        preserved := Set.add !preserved v;
        if restore then restored := Set.add !restored v;
        (* If it needs to be preserved/restored, but is not live after a call,
           then we can still refer to it by register instead of by stack
           location. *)
        if live_after_call name then begin
          spilled := Set.add !spilled name;
          Hvar.create_with_storage name ~at_entry ~at_exit:None
        end else hvar in
      (* Find which hvars we need to spill. *)
      let* hvars' = KB.List.map hvars ~f:(fun hvar ->
          let name = Hvar.name hvar in
          match Hvar.value hvar with
          | Hvar.Storage {at_entry = Register v; at_exit} -> begin
              match Map.find caller_save v with
              | None -> KB.return hvar
              | Some (offset, _) ->
                match at_exit with
                | Some Hvar.(Register v') when String.(v = v') ->
                  KB.return @@ spill name v offset hvar ~restore:true
                | Some _ ->
                  Err.(fail @@ Other (
                      sprintf "Unexpected value for `at_exit` of higher var %s"
                        name))
                | None ->
                  (* Don't bother to preserve the register if it's not live
                     after a call. *)
                  if live_after_call name
                  then KB.return @@ spill name v offset hvar
                  else KB.return hvar
            end
          | _ -> KB.return hvar) in
      (* Collect the exit blocks. *)
      let* exits = Helper.exit_blks blks in
      let exits = List.map exits ~f:Term.tid |> Tid.Set.of_list in
      (* If we needed to spill or restore, then make sure that other higher
         vars aren't using stack locations. *)
      let no_regs = Set.is_empty !preserved && Set.is_empty !restored in
      let* () =
        if no_regs then KB.return ()
        else check_hvars_for_existing_stack_locations sp hvars in
      (* Do we need to change anything? *)
      if no_regs && sp_align = 0
      then KB.return (blks, hvars', !spilled)
      else
        let mem = Var.reify @@ Theory.Target.data tgt in
        let endian =
          if Theory.(Endianness.(Target.endianness tgt = eb))
          then BigEndian else LittleEndian in
        (* Predetermined amount of space to allocate on the stack. *)
        let space =
          Map.length caller_save * (width lsr 3) |>
          Int.round_up ~to_multiple_of:(Theory.Target.data_alignment tgt) in
        let space = Word.of_int ~width @@
          if no_regs then sp_align else sp_align + space in
        let sp = Substituter.mark_reg sp in
        (* Place a register into a stack location. *)
        let push v =
          let open Bil.Types in
          let off, reg = Map.find_exn caller_save v in
          let reg = Substituter.mark_reg reg in
          let addr = BinOp (PLUS, Var sp, Int off) in
          let+ tid = Theory.Label.fresh in
          Def.create ~tid mem @@ Store (Var mem, addr, Var reg, endian, `r32) in
        (* Load a register from a stack location. *)
        let pop v =
          let open Bil.Types in
          let off, reg = Map.find_exn caller_save v in
          let reg = Substituter.mark_reg reg in
          let addr = BinOp (PLUS, Var sp, Int off) in
          let+ tid = Theory.Label.fresh in
          Def.create ~tid reg @@ Load (Var mem, addr, endian, `r32) in
        (* Create the new defs. *)
        let* pushes =
          Set.to_list !preserved |> List.rev |> KB.List.map ~f:push in
        let* pops = Set.to_list !restored |> List.rev |> KB.List.map ~f:pop in
        (* Insert the new defs into the entry/exit blocks accordingly. *)
        let+ blks = KB.List.map blks ~f:(fun blk ->
            let tid = Term.tid blk in
            if Tid.(tid = Term.tid entry_blk) then
              let blk = List.fold pushes ~init:blk ~f:(fun blk def ->
                  Term.prepend def_t blk def) in
              let+ tid = Theory.Label.fresh in
              let adj = Def.create ~tid sp @@ BinOp (MINUS, Var sp, Int space) in
              Term.prepend def_t blk adj
            else if Set.mem exits tid then
              let blk = List.fold pops ~init:blk ~f:(fun blk def ->
                  Term.append def_t blk def) in
              let+ tid = Theory.Label.fresh in
              let adj = Def.create ~tid sp @@ BinOp (PLUS, Var sp, Int space) in
              Term.append def_t blk adj
            else KB.return blk) in
        blks, hvars', !spilled

  (* If we've specified that we want a higher var to remain in a callee-save
     register for the lifetime of the patch, then we need to tell minizinc
     to never use it in a solution. *)
  let collect_exclude_regs (tgt : Theory.target)
      (hvars : Hvar.t list) : String.Set.t =
    let callee_save =
      Theory.Target.regs tgt ~roles:Theory.Role.Register.[callee_saved] |>
      Set.to_list |>
      List.map ~f:(fun v -> Var.name @@ Var.reify v) |>
      String.Set.of_list in
    List.fold hvars ~init:String.Set.empty ~f:(fun acc hvar ->
        match Hvar.value hvar with
        | Hvar.(Storage {at_entry = Register v; at_exit = Some (Register v')})
          when String.(v = v') && Set.mem callee_save v ->
          String.Set.add acc v
        | _ -> acc)

end

(* Substitution of higher vars. *)
module Subst = struct

  let err msg = Kb_error.fail @@ Kb_error.Higher_vars_not_substituted msg

  exception Subst_err of string

  module Naming = struct

    (* BAP seems to prefix an underscore when it's just `$reg` so we will
       just make it explicit. *)
    let reg_prefix = "_$reg"

    let mark_reg_name (v : string) : string = reg_prefix ^ v

    let mark_reg (v : var) : var =
      let name = Var.name v in
      let typ = Var.typ v in
      Var.create (mark_reg_name name) typ

    (* Find a register with a given name in a target arch. *)
    let mark_reg_exn (tgt : Theory.target) (name : string) : var =
      Theory.Target.regs tgt |>
      Set.find ~f:(fun v -> String.(Theory.Var.name v = name)) |>
      function
      | Some r -> mark_reg @@ Var.reify r
      | None ->
        raise
          (Subst_err
             (Format.sprintf "Register %s not found in target arch!" name))

    let unmark_reg_name (v : string) : string option =
      if String.is_prefix v ~prefix:reg_prefix then
        Some (String.drop_prefix v @@ String.length reg_prefix)
      else None

    let unmark_reg (v : var) : var option =
      let name = Var.name v in
      let typ = Var.typ v in
      match unmark_reg_name name with
      | Some name -> Some (Var.create name typ)
      | None -> None

  end

  let get_mem tgt =
    let mem = Theory.Target.data tgt in
    Var.reify mem

  let size_of_typ (typ : typ) (name : string) : size =
    match typ with
    | Bil.Imm n -> Size.of_int n |> Or_error.ok_exn
    | _ -> raise @@ Subst_err (
        Format.sprintf "Unexpected type for variable: %s" name)

  (* Initialize variables with their `at-entry` values. *)
  let initialize
      (blks : blk term list)
      ~(typeof : string -> typ option)
      ~(entry_tid : tid)
      ~(hvars : Hvar.t list)
      ~(tgt : Theory.target)
      ~(spilled : String.Set.t) : blk term list KB.t =
    let endian =
      let e = Theory.Target.endianness tgt in
      if Theory.Endianness.(e = le) then LittleEndian else BigEndian in
    KB.List.map blks ~f:(fun blk ->
        let tid = Term.tid blk in
        if Tid.(tid = entry_tid) then
          let+ defs =
            KB.List.filter_map hvars ~f:(fun hvar ->
                let name = Hvar.name hvar in
                if Set.mem spilled name then KB.return None
                else match Hvar.value hvar with
                  | Hvar.Constant _ -> KB.return None
                  | Hvar.Storage {at_entry = Register reg; _} -> begin
                      match typeof name with
                      | None -> KB.return None
                      | Some typ ->
                        let lhs = Var.create name typ in
                        let rhs = Naming.mark_reg_exn tgt reg in
                        let+ tid = Theory.Label.fresh in
                        Some (Def.create ~tid lhs @@ Var rhs)
                    end
                  | Hvar.Storage {at_entry = Memory memory; _} -> begin
                      match typeof name with
                      | None -> KB.return None
                      | Some typ ->
                        let lhs = Var.create name typ in
                        let mem = get_mem tgt in
                        let size = size_of_typ typ name in
                        let rhs = match memory with
                          | Hvar.Global addr ->
                            Bil.(load ~mem:(var mem) ~addr:(int addr)
                                   endian size)
                          | Hvar.Frame (loc, off) ->
                            let loc = Naming.mark_reg_exn tgt loc in
                            Bil.(load ~mem:(var mem) ~addr:(var loc + int off)
                                   endian size) in
                        let+ tid = Theory.Label.fresh in
                        Some (Def.create ~tid lhs rhs)
                    end) in
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.prepend def_t blk def)
        else KB.return blk)

  (* Finalize variables with their `at-exit` values. *)
  let finalize
      (blks : blk term list)
      ~(typeof : string -> typ option)
      ~(exit_tids : Tid.Set.t)
      ~(hvars : Hvar.t list)
      ~(tgt : Theory.target) : blk term list KB.t =
    let endian =
      let e = Theory.Target.endianness tgt in
      if Theory.Endianness.(e = le) then LittleEndian else BigEndian in
    KB.List.map blks ~f:(fun blk ->
        let tid = Term.tid blk in
        if Set.mem exit_tids tid then
          let+ defs =
            KB.List.filter_map hvars ~f:(fun hvar ->
                let name = Hvar.name hvar in
                match Hvar.value hvar with
                | Hvar.Constant _ -> KB.return None
                | Hvar.Storage {at_exit = None; _} -> KB.return None
                | Hvar.Storage {at_exit = Some (Register reg); _} -> begin
                    match typeof name with
                    | None -> KB.return None
                    | Some typ ->
                      let rhs = Bil.var @@ Var.create name typ in
                      let lhs = Naming.mark_reg_exn tgt reg in
                      let+ tid = Theory.Label.fresh in
                      Some (Def.create ~tid lhs rhs)
                  end
                | Hvar.Storage {at_exit = Some (Memory memory); _} -> begin
                    match typeof name with
                    | None -> KB.return None
                    | Some typ ->
                      let rhs = Bil.var @@ Var.create name typ in
                      let mem = get_mem tgt in
                      let lhs = mem in
                      let size = size_of_typ typ name in
                      let rhs = match memory with
                        | Hvar.Global addr ->
                          Bil.(store ~mem:(var mem) ~addr:(int addr)
                                 rhs endian size)
                        | Hvar.Frame (loc, off) ->
                          let loc = Naming.mark_reg_exn tgt loc in
                          Bil.(store ~mem:(var mem) ~addr:(var loc + int off)
                                 rhs endian size) in
                      let+ tid = Theory.Label.fresh in
                      Some (Def.create ~tid lhs rhs)
                  end) in
          List.fold defs ~init:blk ~f:(fun blk def ->
              Term.append def_t blk def)
        else KB.return blk)

  (* Substitute the name with an appropriate expression. Since we have the
     initialization and finalization done, we only need to worry about
     constants. *)
  let subst_name
      (name : string)
      ~(typ : typ)
      ~(hvar : Hvar.t) : exp =
    match Hvar.value hvar with
    | Hvar.Constant const -> Bil.int const
    | Hvar.Storage _ -> Bil.var @@ Var.create name typ

  let subst_var (v : var) ~(hvars : Hvar.t list) : exp =
    let name = Var.name v in
    let typ = Var.typ v in
    match Hvar.find name hvars with
    | Some hvar -> subst_name name ~typ ~hvar
    | None -> Bil.var v

  let subst_exp (e : exp) ~(hvars : Hvar.t list) : exp =
    let obj = object
      inherit Exp.mapper
      method! map_var v = subst_var v hvars
    end in
    obj#map_exp e

  let subst_def (def : def term) ~(hvars : Hvar.t list) : def term =
    let lhs = Def.lhs def in
    let name = Var.name lhs in
    let rhs = Def.rhs def |> subst_exp ~hvars in
    match Hvar.find name hvars with
    | None -> Def.with_rhs def rhs
    | Some hvar -> match Hvar.value hvar with
      | Hvar.Storage _ -> Def.with_rhs def rhs
      | Hvar.Constant _ -> raise @@ Subst_err (
          sprintf "Higher var %s appeared on the LHS of a def, but is \
                   given a constant value" name)

  let subst_label
      (label : label)
      ~(hvars : Hvar.t list)
      ~(tgt : Theory.target) : label KB.t = match label with
    | Indirect _ -> KB.return label
    | Direct tid ->
      let+ name = KB.collect Theory.Label.name tid in
      match name with
      | None -> label
      | Some name -> match Hvar.find name hvars with
        | None -> label
        | Some hvar ->
          let typ = Type.Imm (Theory.Target.code_addr_size tgt) in
          Indirect (subst_name name ~typ ~hvar)

  let subst_dsts
      (jmp : jmp term)
      ~(hvars : Hvar.t list)
      ~(tgt : Theory.target) : jmp term KB.t =
    let tid = Term.tid jmp in
    let cond = Jmp.cond jmp in
    match Jmp.kind jmp with
    | Goto label ->
      let+ label = subst_label label ~hvars ~tgt in
      Jmp.create_goto ~cond ~tid label
    | Call call -> begin
        let* target = Call.target call |> subst_label ~hvars ~tgt in
        match Call.return call with
        | None ->
          let call = Call.create ~target () in
          KB.return @@ Jmp.create_call ~cond ~tid call 
        | Some return ->
          let+ return = subst_label return ~hvars ~tgt in
          let call = Call.create ~target ~return () in
          Jmp.create_call ~cond ~tid call
      end
    | Ret label ->
      let+ label = subst_label label ~hvars ~tgt in
      Jmp.create_ret ~cond ~tid label
    | Int _ -> KB.return jmp

  let subst_jmp
      (jmp : jmp term)
      ~(hvars : Hvar.t list)
      ~(tgt : Theory.target) : jmp term KB.t =
    Jmp.map_exp jmp ~f:(subst_exp ~hvars) |>
    subst_dsts ~hvars ~tgt

  let subst_blk
      (blk : blk term)
      ~(hvars : Hvar.t list)
      ~(tgt : Theory.target) : blk term KB.t =
    let+ jmps =
      Term.enum jmp_t blk |> Seq.to_list |>
      KB.List.map ~f:(subst_jmp ~hvars ~tgt) in
    (* This pass should be run before we convert to SSA form, so
       just do nothing with the phi nodes. *)
    let phis = Term.enum phi_t blk |> Seq.to_list in
    let defs =
      Term.enum def_t blk |> Seq.to_list |>
      List.map ~f:(subst_def ~hvars) in
    Blk.create () ~phis ~defs ~jmps ~tid:(Term.tid blk)

  let substitute
      ?(spilled : String.Set.t = String.Set.empty)
      (blks : blk term list)
      ~(entry_tid : tid)
      ~(hvars : Hvar.t list)
      ~(tgt : Theory.target) : blk term list KB.t =
    try
      let* exit_tids =
        let+ exits = Helper.exit_blks blks in
        List.map exits ~f:Term.tid |> Tid.Set.of_list in
      let typeof =
        let env = List.fold blks ~init:String.Map.empty ~f:(fun env blk ->
            let free = Blk.free_vars blk in
            let lhs =
              Term.enum def_t blk |>
              Seq.fold ~init:Var.Set.empty ~f:(fun lhs def ->
                  Set.add lhs @@ Def.lhs def) in
            Var.Set.union free lhs |>
            Set.fold ~init:env ~f:(fun env v ->
                Map.set env ~key:(Var.name v) ~data:(Var.typ v))) in
        (* NOTE: the variable might not have been mentioned in the
           program. *)
        Map.find env in
      let* blks =
        initialize blks ~typeof ~entry_tid ~hvars ~tgt ~spilled in
      let* blks =
        finalize blks ~typeof ~exit_tids ~hvars ~tgt in
      KB.List.map blks ~f:(subst_blk ~hvars ~tgt)
    with Subst_err msg -> err msg

end

(* VIBES IR requires linear SSA form. *)
module Ssa = struct

  let linear
      (patch : Data.Patch.t)
      (sub : sub term) : blk term list KB.t =
    sub |> Sub.ssa |> Linear_ssa.transform ~patch:(Some patch)

end

let run (patch : Data.Patch.t) : t KB.t =
  let* code = Data.Patch.get_bir patch in
  let info_str = Format.asprintf "\nPatch: %a\n\n%!" KB.Value.pp code in
  Events.(send @@ Info info_str);
  let* tgt = Data.Patch.get_target patch in
  let* sp_align = Data.Patch.get_sp_align_exn patch in
  let* hvars = Data.Patch.get_patch_vars_exn patch in
  (* BAP will give us the blks in such an order that the first one is the
     entry blk. *)
  let ir = Blk.from_insns [code] in
  let* entry_blk = match ir with
    | blk :: _ -> KB.return blk
    | [] -> Err.(fail @@ Other (
        "Bir_passes: Blk.from_insns returned an empty list of blks")) in
  (* ABI passes. *)
  let* argument_tids = ABI.collect_argument_tids ir in
  let* ir, mem_argument_tids = ABI.insert_new_mems_at_callsites tgt ir in
  let argument_tids = Tid.Set.union argument_tids mem_argument_tids in
  (* This is needed for inserting a common exit block, where we will
     readjust the stack if necessary. It is also needed for blocks with
     no jumps. They must be reordered to the end of the program so that
     they don't implicitly fall through to another block. *)
  let* ir = Shape.remove_unreachable ir @@ Term.tid entry_blk in
  let* ir = Shape.adjust_exits ir in
  let* ir, hvars, spilled = ABI.spill_hvars_and_adjust_stack ir
      ~tgt ~sp_align ~hvars ~entry_blk in
  let exclude_regs = ABI.collect_exclude_regs tgt hvars in
  (* Substitute higher vars. *)
  let* ir = Subst.substitute ir
      ~entry_tid:(Term.tid entry_blk) ~hvars ~tgt ~spilled in
  (* Optimization. *)
  let* ir = Shape.reorder_blks ir in
  let* ir = Opt.apply ir in
  let* sub = Helper.create_sub ir in
  let sub = Opt.Bap_opt.run sub in
  (* List.iter ir ~f:(Format.eprintf "%a\n%!" Blk.pp); *)
  (* Format.eprintf "-----------------------------------\n%!"; *)
  (* let* () = Kb_error.fail @@ Other "" in *)
  let cfg = Sub.to_graph sub in
  (* Linear SSA form is needed for VIBES IR. *)
  let* ir = Ssa.linear patch sub in
  KB.return {ir; cfg; exclude_regs; argument_tids}
