open Core
open Bap.Std
open Bap_core_theory
open Vibes_ir.Types

open KB.Syntax

type dsts = {
  dst : Operand.t;
  ret : Operand.t option;
}

let get_const (v : 'a Theory.Bitv.t Theory.value) : word option =
  match KB.Value.get Exp.slot v with
  | Int w -> Some w
  | _ -> None

let get_label (tid : tid) : Operand.t KB.t =
  let+ addr = KB.collect Theory.Label.addr tid in
  match addr with
  | None -> Operand.Label tid
  | Some addr ->
    let w = Bitvec.to_int addr |> Word.of_int ~width:32 in
    Operand.Offset w

let get_dsts (jmp : jmp term) : dsts option KB.t =
  let aux dst = match Jmp.resolve dst with
    | First dst -> get_label dst >>| Option.return
    | Second c -> KB.return @@ Option.map
        ~f:(fun w -> Operand.Offset w) (get_const c) in
  match Jmp.dst jmp, Jmp.alt jmp with
  | Some dst, None ->
    aux dst >>| begin function
      | Some dst -> Some {dst; ret = None}
      | None -> None
    end
  | Some dst, Some alt -> begin
      aux dst >>= function
      | None -> !!None
      | Some ret ->  aux alt >>| function
        | Some dst -> Some {dst; ret = Some ret}
        | None -> None
    end
  | _ -> !!None
