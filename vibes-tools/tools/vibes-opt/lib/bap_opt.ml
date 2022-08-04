(* This code is taken directly from BAP's optimization passes:

   https://github.com/BinaryAnalysisPlatform/bap/tree/master/plugins/optimization

   Since this is a plugin and not a library, it's not exposed for
   programmatic use.

   For our purposes, we will use an optimization level of 1, so
   only virtual variables will be touched.
*)

open Core
open Bap.Std
open Graphlib.Std

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
