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

(* This is a bit of a hack since register coalescing is a hit-or-miss. *)
let remove_redundant_moves
    (ir : t)
    ~(is_move : Operation.t -> bool) : t =
  (* Keep this intra-block for now. *)
  map_blks ir ~f:(fun blk ->
      let users = Block.users_map blk in
      let oo = Block.operand_to_operation blk in
      let remove =
        List.filter blk.data ~f:is_move |>
        List.fold ~init:Int.Set.empty ~f:(fun acc (o : Operation.t) ->
            (* Already removed? *)
            if not @@ Set.mem acc o.id then
              (* This shouldn't fail if `is_move` is defined correctly. *)
              match List.hd_exn o.lhs, List.hd_exn o.operands with
              | Var l, Var r -> begin
                  (* Exactly one user. *)
                  match Map.find users @@ List.hd_exn l.temps with
                  | Some [v] -> begin
                      (* Is it also a move operation? *)
                      match Map.find oo v.id with
                      | Some u when is_move u -> begin
                          match List.hd_exn u.lhs with
                          | Var l -> begin
                              (* Check if we had a pattern of `l -> r -> l`,
                                 making this sequence of moves redundant. *)
                              match l.preassign, r.preassign with
                              | Some l, Some r when Var.same l r ->
                                Set.add (Set.add acc o.id) u.id
                              | _ -> acc
                            end
                          | _ -> acc
                        end
                      | Some _ | None -> acc
                    end
                  | Some _ | None -> acc
                end
              | _ -> acc
            else acc) in
      let data = List.filter blk.data ~f:(fun o ->
          not @@ Set.mem remove o.id) in
      {blk with data})

let peephole
    (ir : t)
    ~(is_nop : Operation.t -> bool)
    ~(unconditional_branch_target : Operation.t -> tid option)
    ~(is_move : Operation.t -> bool) : t =
  let ir = map_blks ir ~f:(filter_nops ~is_nop) in
  let ir = create_implicit_fallthroughs ir ~unconditional_branch_target in
  let ir = remove_redundant_moves ir ~is_move in
  let ir = filter_empty_blocks ir in
  ir
