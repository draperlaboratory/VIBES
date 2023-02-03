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

let is_nop (op : Operation.t) : bool =
  let Operation.{opcodes; lhs; operands; _} = op in
  List.exists opcodes ~f:(function
      | "mov" | "movs" -> begin
          match lhs, operands with
          | [Var a1], [Var a2] -> begin
              match a1.preassign, a2.preassign with
              | Some v1, Some v2 -> Var.(v1 = v2)
              | _ -> false
            end
          | _ -> false
        end
      | "add" | "adds" | "sub" | "subs" -> begin
          match lhs, operands with
          | [Var a1], [Var a2; Const w] -> begin
              match a1.preassign, a2.preassign with
              | Some v1, Some v2 -> Var.(v1 = v2) && Word.is_zero w
              | _ -> false
            end
          | _ -> false
        end
      | "and" | "orr" -> begin
          match lhs, operands with
          | [Var a1], [Var a2] -> begin
              match a1.preassign, a2.preassign with
              | Some v1, Some v2 -> Var.(v1 = v2)
              | _ -> false
            end
          | _ -> false
        end
      | _ -> false)

let unconditional_branch_target (op : Operation.t) : tid option =
  List.find_map op.opcodes ~f:(function
      | "b" -> List.find_map op.operands ~f:(function
          | Label tid -> Some tid
          | _ -> None)
      | _ -> None)

let is_move (op : Operation.t) : bool =
  List.exists op.opcodes ~f:(function
      | "mov" | "movs" -> begin
          match op.lhs, op.operands with
          | [Var _], [Var _] -> true
          | _ -> false
        end
      | _ -> false)
