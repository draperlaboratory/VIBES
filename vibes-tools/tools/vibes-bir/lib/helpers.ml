open Core
open Bap.Std

module Err = Vibes_error_lib.Std

let spill_tag = Value.Tag.register (module Unit)
  ~name:"spilled"
  ~uuid:"a770b736-07d1-11ed-84a7-7f2a318d4806"

(* Create a dummy subroutine from the blocks. *)
let create_sub (blks : blk term list) : sub term =
  let tid = Tid.create () in
  Sub.create ~name:"dummy-wrapper" ~blks ~tid ()

let is_implicit_exit (blk : blk term) : bool =
  Seq.is_empty @@ Term.enum jmp_t blk

(* Returns true if the jmp is unconditional. *)
let is_unconditional (jmp : jmp term) : bool =
  match Jmp.cond jmp with
  | Int w -> Word.(w = b1)
  | _ -> false

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
     this block will not be returning to the patch code. *)
let exit_blks (blks : blk term list) : (blk term list, Err.t) result =
  match blks with
  | [] ->
     let msg = "Bir_helpers.exit_blks: got an empty list of blks" in
     Error (Types.No_blks msg)
  | [_] -> Ok blks
  | _ ->
    let sub = create_sub blks in
    let cfg = Sub.to_cfg sub in
    let nodes_seq = Graphs.Ir.nodes cfg in
    let nodes = Seq.to_list nodes_seq in
    let blks = List.filter_map nodes ~f:(fun node ->
        let blk = Graphs.Ir.Node.label node in
        if Graphs.Ir.Node.degree node cfg ~dir:`Out = 0
        then Some blk
        else match Term.enum jmp_t blk |> Seq.to_list with
          | [] -> Some blk
          | [jmp] when not @@ is_unconditional jmp -> Some blk
          | jmps ->
            if List.exists jmps ~f:(fun jmp ->
                match Jmp.kind jmp with
                | Call call -> begin
                    match Call.return call with
                    | None | Some (Indirect _) -> true
                    | _ -> false
                  end
                | _ -> false)
            then Some blk
            else None)
    in
    Ok blks

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
