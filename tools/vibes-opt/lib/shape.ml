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
open Graphlib.Std
open Bap_core_theory

module T = Theory
module Patch_info = Vibes_patch_info.Types
module Spaces = Patch_info.Spaces
module Bir_helpers = Vibes_bir.Helpers
module Tags = Vibes_bir.Tags

open KB.Syntax

module G = Graphs.Tid

(* Remove any unreachable blocks. *)
let remove_unreachable (sub : sub term) : (sub term, KB.conflict) result =
  Bir_helpers.entry_tid sub |> Result.map ~f:(fun entry_tid ->
      let cfg = Sub.to_graph sub in
      Term.filter blk_t sub ~f:(fun blk ->
          let tid = Term.tid blk in
          Tid.(tid = entry_tid) ||
          let preds =
            G.Node.preds tid cfg |> Seq.to_list |> Tid.Set.of_list in
          not @@ Tid.Set.(is_empty @@ remove preds G.start)))

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
let adjust_exits (sub : sub term) : sub term KB.t =
  match Term.first blk_t sub with
  | None ->
    let msg = "Vibes_opt.Shape.adjust_exits: got an empty list of blks" in
    KB.fail @@ Errors.No_blks msg
  | Some blk when Term.length blk_t sub = 1
               && Bir_helpers.no_jmps blk ->
    (* We're already in an acceptable form. Adding an extra block
       could lose us opportunity for peephole optimization. *)
    !!sub
  | Some _ -> match Bir_helpers.exit_blks sub with
    | Error err -> KB.fail err
    | Ok exits ->
      let exit_tids = List.map exits ~f:Term.tid |> Tid.Set.of_list in
      (* This is the block that we may insert as a continuation
         for `call ... with noreturn`, as well as blocks with no jumps
         at all. Since such blocks have no particular successor in the
         CFG, this block can be shared between them. *)
      let extra = ref None in
      let make_extra () = match !extra with
        | Some blk -> !!(Term.tid blk)
        | None ->
          let+ tid = T.Label.fresh in
          extra := Some (Blk.create ~tid ());
          tid in
      (* Returns to indirect targets, on the other hand, require us
         to make a unique block for each target. *)
      let extra_indirect = ref [] in
      (* Collect information about jmps that need to be rewrtitten. *)
      let+ sub = Term.KB.map blk_t sub ~f:(fun blk ->
          let tid = Term.tid blk in
          if Set.mem exit_tids tid then
            let attrs = Term.attrs blk in
            match Term.enum jmp_t blk |> Seq.to_list with
            | [] ->
              (* The block has no jumps, so it is implicitly an exit node.
                 Make this explicitly jump to a new exit block at the very
                 end of the program. *)
              let* jmp_tid = T.Label.fresh in
              let+ dst = make_extra () in
              let jmp = Jmp.create_goto ~tid:jmp_tid (Direct dst) in
              let defs = Term.enum def_t blk |> Seq.to_list in
              Term.with_attrs
                (Blk.create ~tid ~jmps:[jmp] ~defs ())
                attrs
            | [jmp] when not @@ Bir_helpers.is_unconditional jmp ->
              (* The block has a single conditional jump, so it is implicitly
                 an exit node. Make this explicitly jump to a new exit block
                 at the very end of the program. *)
              let* jmp_tid = T.Label.fresh in
              let+ dst = make_extra () in
              let new_jmp = Jmp.create_goto ~tid:jmp_tid (Direct dst) in
              let defs = Term.enum def_t blk |> Seq.to_list in
              Term.with_attrs
                (Blk.create ~tid ~jmps:[jmp; new_jmp] ~defs ())
                attrs
            | _ -> Term.KB.map jmp_t blk ~f:(fun jmp ->
                match Jmp.kind jmp with
                | Call call -> begin
                    match Call.return call with
                    | Some (Direct _) -> !!jmp
                    | Some (Indirect _ as label) ->
                      (* We need a unique exit node that has an indirect
                         `goto label`. *)
                      let+ tid = T.Label.fresh in
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
                | _ -> !!jmp)
          else !!blk) in
      (* Append the generated blocks. *)
      Option.value_map !extra
        ~default:!extra_indirect
        ~f:(fun e -> !extra_indirect @ [e]) |>
      List.fold ~init:sub ~f:(Term.append blk_t)

(* Order the blocks according to a reverse postorder DFS traversal.
   This should minimize the number of extra jumps we need to insert. *)
let reorder_blks (sub : sub term) : sub term =
  let cfg = Sub.to_cfg sub in
  let blks =
    Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg |>
    Seq.to_list |> List.map ~f:Graphs.Ir.Node.label in
  (* The exit block with no jmps should be ordered at the very end of
     the program. *)
  let blks = match List.find blks ~f:Bir_helpers.no_jmps with
    | None -> blks
    | Some blk ->
      let tid = Term.tid blk in
      let blks = List.filter blks ~f:(fun blk' ->
          Tid.(tid <> Term.tid blk')) in
      blks @ [blk] in
  let args = Term.enum arg_t sub |> Seq.to_list in
  let name = Sub.name sub in
  let tid = Term.tid sub in
  let attrs = Term.attrs sub in
  Term.with_attrs (Sub.create () ~args ~blks ~name ~tid) attrs

(* This is only meant for when we generate Thumb code.

   Example:

   x := a + 1
   y := z
   when x < z goto %00001234

   Gets compiled to:

   movs x, a
   adds x, 1
   movs y, z
   cmp x, z
   bls blk00001234

   Many simple instructions (data transfer, arithmetic) on Thumb will
   set the flags if they have a two-byte encoding. Unfortunately, we
   don't currently model these effects on the flags in the instruction
   selector (or our Minizinc model), so the scheduler might actually
   come up with this arrangement:

   movs x, a
   adds x, 1
   cmp x, y
   movs y, z
   bls blk00001234

   The `movs y, z` has now clobbered the flags and will change the
   result of the branch taken. This pass will transform the code to:

   movs x, a
   adds x, 1
   movs y, z
   b split
   split:
   cmp x, z
   bls blk00001234

   Here, the block `split` exists only to evaluate the condition and
   then perform the branch. Later we can remove the `b split` instruction
   in the peephole optimizer since we can replace it with a fallthrough.
   By making this control transfer explicit we prevent our Minizinc model
   from changing the ordering of the comparison instruction.
*)
let split_on_conditional (sub : sub term) : sub term KB.t =
  let after = Tid.Table.create () in
  let is_cond = Fn.non Bir_helpers.is_unconditional in
  let+ sub = Term.KB.map blk_t sub ~f:(fun blk ->
      let jmps = Term.enum jmp_t blk |> Seq.to_list in
      if List.exists jmps ~f:is_cond then
        let tid = Term.tid blk in
        let* jmp_tid = T.Label.fresh in
        let+ blk_tid = T.Label.fresh in
        let cont = Blk.create () ~tid:blk_tid ~jmps in
        let cont = Term.set_attr cont Tags.split () in
        Tid.Table.set after ~key:tid ~data:cont;
        let defs = Term.enum def_t blk |> Seq.to_list in
        let jmp = Jmp.create ~tid:jmp_tid @@ Goto (Direct blk_tid) in
        let dict = Term.attrs blk in
        let blk = Blk.create () ~tid ~defs ~jmps:[jmp] in
        Term.with_attrs blk dict
      else !!blk) in
  Tid.Table.fold after ~init:sub
    ~f:(fun ~key:after ~data:blk sub ->
        Term.append blk_t sub blk ~after)

(* If BAP gave us a block with a call of the form:

   `when cond call x with return y`

   Then we need to turn this into:

   `when cond goto z`

   Where `z` contains:

   `call x with return y`
*)
let split_conditional_calls (sub : sub term) : sub term KB.t =
  let after = Tid.Table.create () in
  let+ sub = Term.KB.map blk_t sub ~f:(fun blk ->
      Term.KB.map jmp_t blk ~f:(fun jmp -> match Jmp.kind jmp with
          | Call call when not @@ Bir_helpers.is_unconditional jmp ->
            let* blk_tid = T.Label.fresh in
            let+ jmp_tid = T.Label.fresh in
            let new_jmp = Jmp.create_call ~tid:jmp_tid call in
            let new_blk = Blk.create () ~tid:blk_tid ~jmps:[new_jmp] in
            let new_blk = Term.set_attr new_blk Tags.split () in
            Tid.Table.set after ~key:(Term.tid blk) ~data:new_blk;
            Term.with_attrs
              (Jmp.create_goto (Direct blk_tid)
                 ~cond:(Jmp.cond jmp)
                 ~tid:(Term.tid jmp))
              (Term.attrs jmp)
          | _ -> !!jmp)) in
  Tid.Table.fold after ~init:sub
    ~f:(fun ~key:after ~data:blk sub ->
        Term.append blk_t sub blk ~after)
