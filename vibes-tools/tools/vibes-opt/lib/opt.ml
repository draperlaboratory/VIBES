open Core
open Bap.Std
open Graphlib.Std
open Bap_core_theory

module Hvar = Vibes_higher_vars.Higher_var
module Helper = Vibes_bir.Helpers

type t = Hvar.t list -> sub term -> sub term KB.t

module type S = sig val go : t end

module G = Graphs.Tid

open KB.Syntax

module Builtin : S = struct

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

  end

  let is_optimization_allowed hvars var =
    Var.is_virtual var || match Hvar.find Var.(name @@ base var) hvars with
    | Some {value = Registers {allow_opt; _}; _} -> allow_opt
    | Some _ | None -> false

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

  exception Ill_typed of string

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
    let e = substituter#map_exp exp in
    try Exp.fold_consts e
    with _ ->
      let msg = Format.asprintf
          "Vibes_opt.Opt.Builtin.substitute: expression %a is not \
           well-formed" Exp.pp e in
      raise @@ Ill_typed msg

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
    let+ sub' = Sub.KB.ssa sub in
    let sub', dead = loop Tid.Set.empty sub' in
    O.create ~deads:dead sub'

  let go hvars sub = try
      let free = Sub.free_vars sub in
      let can_touch = is_optimization_allowed hvars in
      let+ data = process_sub free can_touch sub in
      let sub = O.update sub data in
      let data = O.find_unreachable sub data in
      O.remove_dead_code sub data
    with Ill_typed msg -> KB.fail @@ Errors.Invalid_bir msg

end

module Merge : S = struct

  (* We can merge with a successor block if we are its only predecessor.
     Note that the pseudo-start node will be a predecessor of "unreachable"
     blocks. Furthermore, the implicit exit block cannot be merged with.

     We remove the pseudo-entry node to get the real predecessor count.
  *)
  let can_merge (tid : tid) (exit_tid : tid option) (cfg : G.t) : bool =
    not (Option.exists exit_tid ~f:(Tid.equal tid)) &&
    let preds =
      G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
    Set.(length @@ remove preds G.start) = 1

  let rec loop (sub : sub term) : sub term =
    with_cfg (Sub.to_graph sub) sub

  and with_cfg (cfg : G.t) (sub : sub term) : sub term =
    let blks = Term.enum blk_t sub in
    (* Map tids to blocks. *)
    let blk_table =
      Seq.fold blks ~init:Tid.Map.empty ~f:(fun tbl blk ->
          Map.set tbl ~key:(Term.tid blk) ~data:blk) in
    (* Get the implicit exit block. *)
    let exit_tid = Seq.find_map blks ~f:(fun blk ->
        Option.some_if (Helper.no_jmps blk) (Term.tid blk)) in
    let merged = Tid.Hash_set.create () in
    let sub = Term.filter_map blk_t sub ~f:(fun blk ->
        let tid = Term.tid blk in
        if Hash_set.mem merged tid then None
        else match Term.enum jmp_t blk |> Seq.to_list with
          | [jmp] when Helper.is_unconditional jmp -> begin
              match Jmp.kind jmp with
              | Goto (Direct tid') when Tid.(tid <> tid') ->
                if can_merge tid' exit_tid cfg then
                  (* This must run before the SSA pass, so there should
                     not be any phi nodes in the program. *)
                  let blk' = Map.find_exn blk_table tid' in
                  let defs = Term.enum def_t blk |> Seq.to_list in
                  let defs' = Term.enum def_t blk' |> Seq.to_list in
                  let jmps' = Term.enum jmp_t blk' |> Seq.to_list in
                  let attrs = Term.attrs blk in
                  let blk = Blk.create ()
                      ~tid ~defs:(defs @ defs') ~jmps:jmps' in
                  Hash_set.add merged tid';
                  Some (Term.with_attrs blk attrs)
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
    if Hash_set.is_empty merged then sub else loop sub

  (* If two blocks have a single, unconditional edge in between them,
     then they can be merged together. This requires a particular
     ordering for the blocks, since blocks that get merged into their
     predecessors will get deleted. Therefore, they must be visited
     afterwards in the ordering. *)
  let go : t = fun _ sub -> !!(loop sub)

end

module Contract : S = struct

  (* Collect all blocks that contain only a single unconditional Goto.
     In other words, these blocks are just "middlemen" and can thus be
     "cut out". Predecessors of these blocks can then have their
     control flow redirected to the successors of these "middlemen". *)
  let collect_singles (sub : sub term) (entry_tid : tid) : label Tid.Map.t =
    Term.enum blk_t sub |> Seq.filter_map ~f:(fun blk ->
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
    Tid.Map.of_sequence_exn

  (* If `tid` is a "single", and it is part of an arbitrarily long chain
     of "singles", then we can try to chase down the final destination.
     Since we may encounter a cycle, we carry around the set of visited
     nodes. *)
  let rec find_single_dst
      ?(visited : Tid.Set.t = Tid.Set.empty)
      (tid : tid)
      (singles : label Tid.Map.t) : label option =
    if Set.mem visited tid then None
    else
      let visited = Set.add visited tid in
      Map.find singles tid |> Option.bind ~f:(function
          | Indirect _ as ind -> Some ind
          | Direct tid' as dir ->
            match find_single_dst tid' singles ~visited with
            | Some _ as next -> next
            | None -> Some dir)

  let rec loop (sub : sub term) (entry_tid : tid) : sub term =
    let singles = collect_singles sub entry_tid in
    let contracted = Tid.Hash_set.create () in
    let changed = ref false in
    let sub = Term.map blk_t sub ~f:(fun blk ->
        let tid = Term.tid blk in
        (* Ignore "singles" that were part of a contraction on
           this iteration. We will keep them in the IR until they
           are no longer reachable. *)
        if Hash_set.mem contracted tid then blk
        else Term.map jmp_t blk ~f:(fun jmp ->
            match Jmp.kind jmp with
            | Goto (Direct tid') when Tid.(tid <> tid') -> begin
                match find_single_dst tid' singles with
                | Some (Direct tid'') when Tid.(tid' <> tid'') ->
                  Hash_set.add contracted tid';
                  changed := true;
                  Jmp.with_kind jmp @@ Goto (Direct tid'')
                | Some (Indirect _ as ind) ->
                  Hash_set.add contracted tid';
                  changed := true;
                  Jmp.with_kind jmp @@ Goto ind
                | None | Some (Direct _) -> jmp
              end
            | _ -> jmp)) in
    (* Repeat until we reach a fixed point. *)
    if !changed then
      (* Remove unreachable nodes before iterating again. *)
      let cfg = Sub.to_graph sub in
      let sub = Term.filter blk_t sub ~f:(fun blk ->
          let tid = Term.tid blk in
          if Hash_set.mem contracted tid then
            let preds =
              G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
            not @@ Tid.Set.(is_empty @@ remove preds G.start)
          else true) in
      loop sub entry_tid
    else sub

  (* Edge contraction: https://en.wikipedia.org/wiki/Edge_contraction *)
  let go : t = fun _ sub -> match Helper.entry_tid sub with
    | Ok entry_tid -> !!(loop sub entry_tid)
    | Error err -> KB.fail err

end

module Nop : S = struct

  let go _ sub = !!sub

end

let passes : (module S) Vector.t =
  Vector.create ~capacity:16 (module Nop : S)

let register : (module S) -> unit = Vector.append passes

let () =
  register (module Builtin);
  register (module Merge);
  register (module Contract)

let apply hvars sub (module O : S) = O.go hvars sub

let apply : t = fun hvars sub ->
  Vector.to_list passes |> KB.List.fold ~init:sub ~f:(apply hvars)
