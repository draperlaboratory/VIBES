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

open Core
open Bap.Std
open Vibes_ir.Types

let filter_empty_blocks (ir : t) : t =
  let tids =
    List.fold ir.blks ~init:Tid.Set.empty ~f:(fun acc blk ->
        Set.add acc blk.tid) in
  let referenced =
    List.fold ir.blks ~init:Tid.Set.empty ~f:(fun init blk ->
        Block.all_rhs_operands blk |>
        List.fold ~init ~f:(fun acc -> function
            | Operand.Label tid -> Set.add acc tid
            | _ -> acc)) |>
    Set.inter tids in
  let blks = List.filter ir.blks ~f:(fun blk ->
      match blk.ctrl, blk.data with
      | [], [] -> Set.mem referenced blk.tid
      | _ -> true) in
  {ir with blks}

let filter_nops (blk : Block.t) ~(is_nop : Operation.t -> bool) : Block.t =
  {blk with ctrl = List.filter blk.ctrl ~f:(Fn.non is_nop);
            data = List.filter blk.data ~f:(Fn.non is_nop)}

let rec interleave_pairs = function
  | x :: y :: rest -> (x, y) :: interleave_pairs (y :: rest)
  | [] | [_] -> []

let successor_map (ir : t) : Tid.Set.t Tid.Map.t =
  List.fold ir.blks ~init:Tid.Map.empty ~f:(fun init blk ->
      List.fold blk.ctrl ~init ~f:(fun acc o ->
          Operation.operands o |> List.find_map ~f:(function
              | Operand.Label l -> Some l | _ -> None) |>
          Option.value_map ~default:acc ~f:(fun tid ->
              Map.update acc blk.tid ~f:(function
                  | None -> Tid.Set.singleton tid
                  | Some s -> Set.add s tid))))

(* Try to replace direct, unconditional jumps with fallthroughs where
   possible. *)
let create_implicit_fallthroughs
    (ir : t)
    ~(unconditional_branch_target : Operation.t -> tid option) : t =
  let afters =
    interleave_pairs ir.blks |>
    List.fold ~init:Tid.Map.empty ~f:(fun afters (x, y) ->
        Map.set afters ~key:x.Block.tid ~data:y.Block.tid) in
  map_blks ir ~f:(fun blk ->
      (* Find the last control operation. *)
      match List.last blk.ctrl with
      | None -> blk
      | Some o ->
        (* Is it an unconditional branch? *)
        match unconditional_branch_target o with
        | None -> blk
        | Some id ->
          (* Is the target of the branch the immediate next block in
             the ordering? *)
          match Map.find afters blk.tid with
          | Some next when Tid.(id = next) ->
            (* Delete the branch in favor of an implicit fallthrough. *)
            Block.{blk with ctrl = List.drop_last_exn blk.ctrl}
          | _ -> blk)

let congruent (ir : t) (l : Opvar.t) (r : Opvar.t) : bool =
  match Map.find ir.congruences @@ List.hd_exn l.temps with
  | Some s -> Set.mem s @@ List.hd_exn r.temps
  | None -> false

let all_outs (ir : t) : Int.Set.t =
  List.fold ir.blks ~init:Int.Set.empty ~f:(fun acc blk ->
      Set.add acc @@ Operation.id @@ Block.outs blk)

let operations_map (ir : t) : Operation.t Int.Map.t =
  List.fold ir.blks ~init:Int.Map.empty ~f:(fun init blk ->
      Block.all_operations blk |> List.fold ~init ~f:(fun acc o ->
          Map.set acc ~key:(Operation.id o) ~data:o))

(* Search for a user of the opvar that is a simple move. Since users
   and definers are limited to the scope of one block, we traverse
   the CFG using the "out" operations of each block, finding their
   correspoinding "in" operations in the successors. *)
let rec consumers
    (tid : tid)
    (v : Opvar.t)
    ~(ir : t)
    ~(users : Opvar.t list Var.Map.t)
    ~(ops : Operation.t Int.Map.t)
    ~(bins : id Tid.Map.t)
    ~(succs : Tid.Set.t Tid.Map.t)
    ~(outs : Int.Set.t)
    ~(oo : Operation.t Int.Map.t)
    ~(is_move : Operation.t -> bool) : Operation.t list =
  let next = consumers ~ir ~users ~ops ~bins ~succs ~outs ~oo ~is_move in
  match Map.find oo v.id with
  | Some u when is_move u -> [u]
  | Some u ->
    (* Check if the consumer is in an out pseudo-operation. If so, then
       we can follow the data flow to the next consumer. *)
    let id = Operation.id u in
    if Set.mem outs id then match Map.find succs tid with
      | None -> []
      | Some s ->
        Set.to_list s |> List.filter_map ~f:(fun tid ->
            let open Option.Monad_infix in
            Map.find bins tid >>=
            Map.find ops >>= fun o ->
            Operation.lhs o |>
            List.find_map ~f:(function
                | Operand.Var u when congruent ir v u ->
                  Some (tid, List.hd_exn u.temps)
                | _ -> None)) |>
        (* For each of the successors, find exactly one user. *)
        List.concat_map ~f:(fun (tid, t) -> match Map.find users t with
            | Some [v] -> next tid v
            | Some _ | None -> [])
    else []
  | None -> []

(* This is a bit of a hack since register coalescing is a hit-or-miss. *)
let remove_redundant_moves
    (ir : t)
    ~(succs : Tid.Set.t Tid.Map.t)
    ~(is_move : Operation.t -> bool) : t =
  let users = users_map ir in
  let oo = operand_to_operation ir in
  let bins = block_to_ins ir in
  let outs = all_outs ir in
  let ops = operations_map ir in
  let consumers = consumers ~ir ~users ~ops ~bins ~succs ~outs ~oo ~is_move in
  (* Keep this intra-block for now. *)
  let remove =
    List.fold ir.blks ~init:Int.Set.empty ~f:(fun init blk ->
        List.filter blk.data ~f:is_move |>
        List.fold ~init ~f:(fun acc (o : Operation.t) ->
            if not @@ Set.mem acc o.id then
              (* This shouldn't fail if `is_move` is defined correctly. *)
              match List.hd_exn o.lhs, List.hd_exn o.operands with
              | Var l, Var r -> begin
                  (* Exactly one user. *)
                  match Map.find users @@ List.hd_exn l.temps with
                  | Some [v] -> begin
                      (* Is it also a move operation? *)
                      match consumers blk.tid v with
                      | [] -> acc
                      | moves -> List.fold moves ~init:acc ~f:(fun acc u ->
                          match List.hd_exn u.lhs with
                          | Var l -> begin
                              (* Check if we had a pattern of `l -> r -> l`,
                                 making this sequence of moves redundant. *)
                              match l.preassign, r.preassign with
                              | Some l, Some r when Var.same l r ->
                                Set.add (Set.add acc o.id) u.id
                              | _ -> acc
                            end
                          | _ -> acc)
                    end
                  | Some _ | None -> acc
                end
              | _ -> acc
            else acc)) in
  map_blks ir ~f:(fun blk ->
      let data = List.filter blk.data ~f:(fun o ->
          not @@ Set.mem remove o.id) in
      {blk with data})

let peephole
    (ir : t)
    ~(is_nop : Operation.t -> bool)
    ~(unconditional_branch_target : Operation.t -> tid option)
    ~(is_move : Operation.t -> bool) : t =
  let succs = successor_map ir in
  let ir = map_blks ir ~f:(filter_nops ~is_nop) in
  let ir = create_implicit_fallthroughs ir ~unconditional_branch_target in
  let ir = remove_redundant_moves ir ~succs ~is_move in
  let ir = filter_empty_blocks ir in
  ir
