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

open Core_kernel
open Bap.Std
open Bap_core_theory

open KB.Let

let spill_tag = Value.Tag.register (module Unit)
    ~name:"spilled"
    ~uuid:"af955827-635b-47a9-9fc2-28774747c4ec"

(* Create a dummy subroutine from the blocks. *)
let create_sub (blks : blk term list) : sub term KB.t =
  let+ tid = Theory.Label.fresh in
  Sub.create ~name:"dummy-wrapper" ~blks ~tid ()

let is_implicit_exit (blk : blk term) : bool =
  Seq.is_empty @@ Term.enum jmp_t blk

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
