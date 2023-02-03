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
open OUnit2

module Ir = Vibes_ir.Types

let in_op (ts : var list) : Ir.Operation.t = {
  id = Ir.fresh_id ();
  lhs = List.map ts ~f:(fun t -> Ir.Operand.Var (Ir.Opvar.create t));
  opcodes = [];
  optional = false;
  operands = [];
}

let out_op (ts : var list) : Ir.Operation.t = {
  id = Ir.fresh_id ();
  lhs = [];
  opcodes = [];
  optional = false;
  operands = List.map ts ~f:(fun t -> Ir.Operand.Var (Ir.Opvar.create t));
}

let simple_op
    (opcode : Ir.opcode)
    (arg : var)
    (args : var list) : Ir.Operation.t = {
  id = Ir.fresh_id ();
  lhs = [Var (Ir.Opvar.create arg)];
  opcodes = [opcode];
  optional = false;
  operands = List.map args ~f:(fun t -> Ir.Operand.Var (Ir.Opvar.create t));
}

let (:=) lhs opcode = fun args -> simple_op opcode lhs args

let op_var_exn : Ir.Operand.t -> Ir.Opvar.t = function
  | Var v -> v
  | _ -> failwith "expected opvar"

let (vir1 : Ir.t),
    (definer_map1 : Ir.Opvar.t Var.Map.t),
    (user_map1 : Ir.Opvar.t list Var.Map.t),
    (op_opcodes1 : string list Ir.id_map),
    (temps1 : var list),
    (operands1 : int list),
    (_operations1 : int list),
    (_oprnd_temps1 : var list) =
  let t1 = Var.create "_000004ff_t1" (Imm 32) in
  let t2 = Var.create "_000004ff_t2" (Imm 32) in
  let t3 = Var.create "_000004ff_t3" (Imm 32) in
  let mov = "mov" in
  let temps = [t1; t2; t3] in
  let op1 = (t2 := mov) [t1] in
  let op2 = (t3 := mov) [t2] in
  let ops = [op1; op2] in
  let blk1 = Ir.Block.{
      tid = Tid.create ();
      data = ops;
      ctrl = [];
      ins = in_op [t1];
      outs = out_op [t3];
      frequency = 1
    } in
  let definer_map = Var.Map.of_alist_exn [
      t1, op_var_exn @@ List.hd_exn blk1.ins.lhs;
      t2, op_var_exn @@ List.hd_exn op1.lhs;
      t3, op_var_exn @@ List.hd_exn op2.lhs;
    ] in
  let user_map = Var.Map.of_alist_exn [
      t1, List.map op1.operands ~f:op_var_exn;
      t2, List.map op2.operands ~f:op_var_exn;
      t3, List.map blk1.outs.operands ~f:op_var_exn;
    ] in
  let op_opcodes = Int.Map.of_alist_exn [
      op1.id, [mov];
      op2.id, [mov];
      blk1.ins.id, [];
      blk1.outs.id, [];
    ] in
  let oprnd_temps = [t1; t1; t2; t2; t3; t3] in
  let operands =
    List.concat [
      blk1.ins.lhs;
      op1.operands;
      op1.lhs;
      op2.operands;
      op2.lhs;
      blk1.outs.operands;
    ] |> List.map ~f:(fun o -> (op_var_exn o).id) in
  let operations = List.map [blk1.ins; op1; op2; blk1.outs] ~f:(fun o -> o.id) in
  let vir1 = Ir.{
      blks = [blk1];
      congruences = Var.Map.empty;
    } in
  vir1, definer_map, user_map, op_opcodes, temps, operands, operations, oprnd_temps

let _equal_gpr_reg r1 r2 = ARM.compare_gpr_reg r1 r2 = 0

let test_definer1 (_ : test_ctxt) : unit =
  assert_equal
    ~cmp:(Var.Map.equal Ir.Opvar.equal)
    definer_map1
    (Ir.definer_map vir1)

let test_user1 (_ : test_ctxt) : unit  =
  assert_equal
    ~cmp:(Var.Map.equal (List.equal Ir.Opvar.equal))
    user_map1
    (Ir.users_map vir1)

let test_temps1 (_ : test_ctxt) : unit =
  assert_equal
    ~cmp:(Var.Set.equal)
    (Var.Set.of_list temps1)
    (Ir.all_temps vir1)

let test_operands1 (_ : test_ctxt) : unit =
  assert_equal
    ~cmp:(Int.Set.equal)
    (Int.Set.of_list operands1)
    (Ir.all_opvar_ids vir1)

let test_op_opcodes1 (_ : test_ctxt) : unit =
  assert_equal
    ~cmp:(Int.Map.equal (List.equal Ir.equal_opcode))
    op_opcodes1
    (Ir.operation_to_opcodes vir1)

let dummy_reg_alloc (t : Ir.t) : Ir.t = Ir.map_opvars t ~f:(fun v ->
    match v.preassign with
    | Some _ -> v
    | None ->
      let var = List.hd_exn v.temps in
      {v with preassign = Some (Var.create "R0" (Var.typ var))})

let test_dummy_reg_alloc (_ : test_ctxt) : unit =
  let r0 = Var.create ~is_virtual:false ~fresh:false "R0" (Type.Imm 32) in
  let ir_alloc = dummy_reg_alloc vir1 in
  let all_operations =
    ir_alloc.blks |>
    List.concat_map ~f:(fun (b : Ir.Block.t) ->
        b.ins :: b.outs :: b.data @ b.ctrl) in
  let all_operands = List.concat_map all_operations
      ~f:(fun (o : Ir.Operation.t) -> o.lhs @ o.operands) in
  let all_op_vars = List.map all_operands ~f:op_var_exn in
  let all_regs =
    List.map all_op_vars ~f:(fun o -> o.preassign) |>
    List.filter_opt in
  assert_equal ~cmp:(List.equal Var.equal) [r0; r0; r0; r0; r0; r0]
    ~printer:(List.to_string ~f:(Fn.compose Sexp.to_string Var.sexp_of_t))
    all_regs

let suite : test = "Test VIBES IR" >::: [
    "Test Definer map" >:: test_definer1;
    "Test User map"  >:: test_user1;
    "Test All Temps" >:: test_temps1;
    "Test All Operands" >:: test_operands1;
    "Test Operation Instructions Map" >:: test_op_opcodes1;
    "Test Dummy Reg Alloc" >:: test_dummy_reg_alloc;
  ]

let () = match Bap_main.init () with
  | Ok () -> run_test_tt_main suite
  | Error e ->
    Format.eprintf "Failed to initialize BAP: %a"
      Bap_main.Extension.Error.pp e;
    exit 1
