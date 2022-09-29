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

let peephole
    (ir : t)
    ~(is_nop : Operation.t -> bool)
    ~(unconditional_branch_target : Operation.t -> tid option) : t =
  let ir = map_blks ir ~f:(filter_nops ~is_nop) in
  let ir = create_implicit_fallthroughs ir ~unconditional_branch_target in
  let ir = filter_empty_blocks ir in
  ir
