open !Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_vibes
open OUnit2
module KB = Knowledge
module H = Helpers


(* We need to use a specific Ir here because of later uses of Test_ir *)
module Ir = Minizinc.Ir

open Ir


let in_op (ts : Var.t list) : operation =
  let tid = Tid.create () in
  { id = tid;
    lhs = List.map ~f:(fun t -> Var (simple_var t)) ts;
    insns = [];
    optional = false;
    operands = [];
  }

let out_op (ts : Var.t list) : operation =
  let tid = Tid.create () in
  { id = tid;
    lhs = [];
    insns = [];
    optional = false;
    operands = List.map ~f:(fun t -> Var (simple_var t)) ts;
  }

let simple_op' opcode arg args =
  let tid = Tid.create () in
  { id = tid;
    lhs = [Var (simple_var arg)];
    insns = [opcode];
    optional = false;
    operands = List.map ~f:(fun t -> Var (simple_var t)) args;
  }

let (:=) lhs opcode = fun args -> simple_op' opcode lhs args


let vir1,
    definer_map1,
    user_map1,
    op_insns1,
    temps1,
    operands1,
    operations1,
    oprnd_temps1 =
  let t1 = Var.create "t1" (Imm 32) in
  let t2 = Var.create "t2" (Imm 32) in
  let t3 = Var.create "t3" (Imm 32) in
  let temps = [t1; t2; t3] in
  let op1 = (t2 := `MOVi) [t1] in
  let op2 = (t3 := `MOVi) [t2] in
  let ops = [op1; op2] in
  let blk1 : Ir.blk =
    {
      id = Tid.create ();
      data = ops;
      ctrl = [];
      ins = in_op [t1];
      outs = out_op [t3];
      frequency = 1
    } in
  let definer_map = Var.Map.of_alist_exn [
      (t1, op_var_exn (List.hd_exn blk1.ins.lhs));
      (t2, op_var_exn (List.hd_exn op1.lhs));
      (t3, op_var_exn (List.hd_exn op2.lhs)) ] in
  let user_map = Var.Map.of_alist_exn [
      (t1, List.map ~f:op_var_exn op1.operands);
      (t2, List.map ~f:op_var_exn op2.operands);
      (t3, List.map ~f:op_var_exn blk1.outs.operands) ] in
  let op_insns = Tid.Map.of_alist_exn [
      (op1.id, [`MOVi]);
      (op2.id, [`MOVi]);
      ( blk1.ins.id, []);
      (blk1.outs.id, []) ] in
  let oprnd_temps = [ t1; t1; t2; t2; t3; t3 ] in
  let operands = List.map ~f:(fun o -> (op_var_exn o).id)
      (blk1.ins.lhs @ op1.operands @ op1.lhs @ op2.operands @ op2.lhs @ blk1.outs.operands) in
  let operations = List.map ~f:(fun o -> o.id) [blk1.ins; op1; op2; blk1.outs] in
  let vir1 = {
    congruent = [];
    blks = [blk1]
  } in
  (vir1, definer_map, user_map, op_insns, temps, operands, operations, oprnd_temps)


let equal_gpr_reg r1 r2 = ARM.compare_gpr_reg r1 r2 = 0

let test_definer1 _  =
  assert_equal ~cmp:(Var.Map.equal equal_op_var) definer_map1 (Ir.definer_map vir1)
let test_user1 _  =
  assert_equal ~cmp:(Var.Map.equal (List.equal equal_op_var)) user_map1 (Ir.users_map vir1)
let test_temps1 _  =
  assert_equal ~cmp:(Var.Set.equal) (Var.Set.of_list temps1) (Ir.all_temps vir1)
let test_operands1 _  =
  assert_equal ~cmp:(Var.Set.equal) (Var.Set.of_list operands1) (Ir.all_operands vir1)
let test_op_insns1 _  =
  assert_equal ~cmp:(Tid.Map.equal (List.equal Ir.equal_insn)) op_insns1 (Ir.operation_insns vir1)
let test_dummy_reg_alloc _ =
  let ir_alloc = Ir.dummy_reg_alloc vir1 in
  let all_operations = ir_alloc.blks |> List.concat_map
                         ~f:(fun b -> b.ins :: b.outs :: b.data @ b.ctrl)
  in
  let all_operands = all_operations |> List.concat_map ~f:(fun o -> o.lhs @ o.operands) in
  let all_op_vars = List.map ~f:Ir.op_var_exn all_operands in
  let all_regs = List.map all_op_vars ~f:(fun o -> o.pre_assign) |> List.filter_opt in
  assert_equal ~cmp:(List.equal equal_gpr_reg) [`R0; `R0; `R0; `R0; `R0; `R0]
    ~printer:(List.to_string ~f:(fun r -> r|> ARM.sexp_of_gpr_reg |> Sexp.to_string))
    all_regs



let suite = [
  "Test Definer map" >:: test_definer1;
  "Test User map"  >:: test_user1;
  "Test All Temps" >:: test_temps1;
  "Test All Operands" >:: test_operands1;
  "Test Operation Instructions Map" >:: test_op_insns1;
  "Test Dummy Reg Alloc" >:: test_dummy_reg_alloc;
]
