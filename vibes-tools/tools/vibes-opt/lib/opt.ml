open Core
open Bap.Std
open Graphlib.Std

module Err = Vibes_error_lib.Std
module Helper = Vibes_bir_lib.Helpers

open Vibes_error_lib.Let

(* BIR optimizations. All of these passes assume that the blocks are ordered
   according to `Shape.reorder_blks`, and that the program is NOT YET in SSA
   form. *)

type t = blk term list -> (blk term list, Err.t) result

module Contract = struct

  (* Edge contraction: https://en.wikipedia.org/wiki/Edge_contraction *)
  let go : t = fun blks ->
    let module G = Graphs.Tid in
    (* Since we assume the correct ordering of the blocks, the entry tid
       should be the first in the list. *)
    let- entry_tid = match blks with
      | [] ->
        let msg = "Opt.Contract: no entry block found" in
        Error (Types.No_blks msg)
      | blk :: _ -> Ok (Term.tid blk) in
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
        let sub = Helper.create_sub blks in
        let cfg = Sub.to_graph sub in
        loop @@ List.filter blks ~f:(fun blk ->
            let tid = Term.tid blk in
            if Set.mem contracted tid then
              let preds =
                G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
              not @@ Tid.Set.(is_empty @@ remove preds G.start)
            else true)
      else Ok blks in
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
  let go : t = fun blks -> Ok (List.map blks ~f:simplify_blk)

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
      let sub = Helper.create_sub blks in
      with_cfg (Sub.to_graph sub) blks
    and with_cfg cfg blks =
      (* Map tids to blocks. *)
      let blk_table = Tid.Table.create () in
      List.iter blks ~f:(fun blk ->
          Tid.Table.set blk_table ~key:(Term.tid blk) ~data:blk);
      (* Get the implicit exit block. *)
      let exit_tid = List.find_map blks ~f:(fun blk ->
          if Helper.is_implicit_exit blk
          then Some (Term.tid blk) else None) in
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
      then Ok blks
      else loop blks in
    loop blks

end

(* Applies all the optimizations in the list *)
let apply_list (opts : t list) : t = fun init ->
  List.fold opts ~init:(Ok init) ~f:(fun ir opt ->
    let- old_ir = ir in
    let- new_ir = opt old_ir in
    Ok (List.append old_ir new_ir))

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
        Map.set updates ~key:(Term.tid d) ~data:(Rhs rhs) in
      let update_jmp updates j =
        let j = Jmp.map_exp ~f:drop_index j in
        let data = Jmp {cond = Jmp.cond j; kind = Jmp.kind j} in
        Map.set updates ~key:(Term.tid j) ~data in
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

    let update sub {updates; _} =
      Term.map blk_t sub ~f:(fun b ->
          Term.map def_t b ~f:(update_def updates) |>
          Term.map jmp_t ~f:(update_jmp updates))

    let filter_map_alive deads cls ?(f=Fn.id) x =
      Term.filter_map cls x ~f:(fun t ->
          if Set.mem deads (Term.tid t) then None
          else Some (f t))

    let remove_dead_code sub {deads; _} =
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
    O.create ~deads:dead sub'

  let run sub =
    (* TODO: [free] should be the result of [Sub.free_vars], 
       but [Sub.free_vars] is raising an error...why? 
       To get the thing to keep running I'm just saying there are no free vars,
       but this is just to make sure the thing compiles and runs...
       Obviously we need to fix this... *)
    let free = Var.Set.empty in (* Sub.free_vars sub in *)
    let can_touch = is_optimization_allowed in
    let data = process_sub free can_touch sub in
    let sub = O.update sub data in
    let data = O.find_unreachable sub data in
    O.remove_dead_code sub data

end
