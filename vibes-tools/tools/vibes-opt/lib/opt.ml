open Core
open Bap.Std
open Bap_core_theory

module Helper = Vibes_bir_lib.Helpers

(* BIR optimizations. All of these passes assume that the blocks are ordered
   according to `Shape.reorder_blks`, and that the program is NOT YET in SSA
   form. *)

type t = sub term -> (sub term, KB.conflict) result

module G = Graphs.Tid

let (let*) x f = Result.bind x ~f
let (let+) x f = Result.map x ~f

module Contract = struct

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
  let go : t = fun sub ->
    let+ entry_tid = Helper.entry_tid sub in
    loop sub entry_tid

end

module Simpl = struct

  exception Ill_typed of string

  (* Keep in mind that `Exp.simpl` will run the BIl type-checker on the
     expression, and it will raise if the expression is not well-typed. *)
  let simplify_exp (e : exp) : exp =
    try Exp.simpl e ~ignore:Eff.[read]
    with _ ->
      let msg = Format.asprintf
          "Vibes_opt.Opt.Simpl.simplify_exp: expression %a is not \
           well-formed" Exp.pp e in
      raise @@ Ill_typed msg

  let simplify_blk (blk : blk term) : blk term =
    Term.map def_t blk ~f:(fun def ->
        Def.with_rhs def @@ simplify_exp @@ Def.rhs def) |>
    Term.map jmp_t ~f:(fun jmp ->
        Jmp.with_cond jmp @@ simplify_exp @@ Jmp.cond jmp)

  let simplify_sub (sub : sub term) : sub term =
    Term.map blk_t sub ~f:simplify_blk

  (* Simplify expressions in blocks. *)
  let go : t = fun sub ->
    try Ok (simplify_sub sub)
    with Ill_typed msg -> Error (Errors.Invalid_bir msg)

end

module Merge = struct

  (* We can merge with a successor block if we are its only predecessor.
     Note that the pseudo-start node will be a predecessor of "unreachable"
     blocks. Furthermore, the implicit exit block cannot be merged with. *)
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
                  let blk = Blk.create ()
                      ~tid ~defs:(defs @ defs') ~jmps:jmps' in
                  Hash_set.add merged tid';
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
    if Hash_set.is_empty merged then sub else loop sub

  (* If two blocks have a single, unconditional edge in between them,
     then they can be merged together. This requires a particular
     ordering for the blocks, since blocks that get merged into their
     predecessors will get deleted. Therefore, they must be visited
     afterwards in the ordering. *)
  let go : t = fun sub -> Ok (loop sub)

end

(* Applies all the optimizations in the list *)
let apply_list (opts : t list) : t = fun sub ->
  List.fold opts ~init:(Ok sub) ~f:(fun sub opt ->
      let* sub = sub in opt sub)

(* Applies all the optimizations we currently perform. *)
let apply : t = apply_list [
    Simpl.go;
    Merge.go;
    Contract.go;
  ]
