open !Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_vibes
open OUnit2
module KB = Knowledge
open Knowledge.Syntax

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


let ex1 : Ir.t = vir1 

(*
let (mzn_params1 , serial_info1) =  Minizinc.serialize_mzn_params ex1

let mzn_params_string = Format.asprintf "%a"
    (Yojson.Safe.pretty_print ~std:true)
    (Minizinc.mzn_params_serial_to_yojson mzn_params1)


let mzn_params_gold =
  "{
  \"reg_t\": {
    \"set\": [
      { \"e\": \"R0\" },
      { \"e\": \"R1\" },
      { \"e\": \"R2\" },
      { \"e\": \"R3\" },
      { \"e\": \"R4\" },
      { \"e\": \"R5\" },
      { \"e\": \"R6\" },
      { \"e\": \"R7\" },
      { \"e\": \"R8\" },
      { \"e\": \"R9\" },
      { \"e\": \"R10\" },
      { \"e\": \"R11\" },
      { \"e\": \"R12\" },
      { \"e\": \"LR\" },
      { \"e\": \"PC\" },
      { \"e\": \"SP\" }
    ]
  },
  \"insn_t\": { \"set\": [ { \"e\": \"MOVi\" } ] },
  \"temp_t\": {
    \"set\": [
      {
        \"e\":
          \"((name(Reg(name t1)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name t2)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name t3)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      }
    ]
  },
  \"operand_t\": {
    \"set\": [
      { \"e\": \"#26\" },
      { \"e\": \"#27\" },
      { \"e\": \"#28\" },
      { \"e\": \"#29\" },
      { \"e\": \"#30\" },
      { \"e\": \"#31\" }
    ]
  },
  \"operation_t\": {
    \"set\": [
      { \"e\": \"%0000000c\" },
      { \"e\": \"%0000000d\" },
      { \"e\": \"%0000000e\" },
      { \"e\": \"%0000000f\" }
    ]
  },
  \"block_t\": { \"set\": [ { \"e\": \"%00000010\" } ] },
  \"class_t\": { \"set\": [ { \"e\": \"unimplemented_class\" } ] },
  \"operand_operation\": [
    { \"e\": \"%0000000c\" },
    { \"e\": \"%0000000c\" },
    { \"e\": \"%0000000d\" },
    { \"e\": \"%0000000d\" },
    { \"e\": \"%0000000e\" },
    { \"e\": \"%0000000f\" }
  ],
  \"definer\": [ { \"e\": \"#31\" }, { \"e\": \"#27\" }, { \"e\": \"#29\" } ],
  \"users\": [
    { \"set\": [ { \"e\": \"#26\" } ] },
    { \"set\": [ { \"e\": \"#28\" } ] },
    { \"set\": [ { \"e\": \"#30\" } ] }
  ],
  \"temp_block\": [
    { \"e\": \"%00000010\" },
    { \"e\": \"%00000010\" },
    { \"e\": \"%00000010\" }
  ],
  \"copy\": { \"set\": [] },
  \"width\": [ 1, 1, 1 ],
  \"preassign\": [
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] }
  ],
  \"congruent\": [
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] },
    { \"set\": [] }
  ],
  \"operation_insns\": [
    { \"set\": [ { \"e\": \"MOVi\" } ] },
    { \"set\": [ { \"e\": \"MOVi\" } ] },
    { \"set\": [] },
    { \"set\": [] }
  ],
  \"latency\": [ 10 ]
}"

let test_serialize_gold _ =
  assert_equal ~cmp:String.equal ~printer:(fun s -> s)
    mzn_params_string
    mzn_params_gold

let test_definer_serialize_ex1 _ =
  assert_equal ~cmp:(Var.Map.equal String.equal)
    (Var.Map.map ~f:(fun o -> Var.to_string o.id) Test_ir.definer_map1)
    (List.zip_exn serial_info1.temps
       (List.map ~f:(fun e -> e.e)
          mzn_params1.definer) |> Var.Map.of_alist_exn)

open Test_ir

let sol1 : Minizinc.sol = {
  reg = Var.Map.of_alist_exn (List.zip_exn temps1 [`R0; `R0; `R0]) ;
  insn = Tid.Map.of_alist_exn (List.zip_exn operations1 [`MOVi; `MOVi; `MOVi; `MOVi]);
  temp = Var.Map.of_alist_exn (List.zip_exn operands1 oprnd_temps1);
  active = Tid.Map.of_alist_exn (List.zip_exn operations1 [true; true; true; true]);
  issue  = Tid.Map.of_alist_exn (List.zip_exn operations1 [4; 3; 2; 1]); (* Just reverse ordered *)
}

let new_vir1 = Minizinc.apply_sol vir1 sol1

(* This is a duplicate of an unexposed function in Vibes_ir *)
let all_operands_helper (blk : blk) : operand list =
  let operation_operands op_list =
    List.concat_map op_list
      ~f:(fun operation ->
          operation.lhs @ operation.operands)
  in
  blk.ins.lhs @
  blk.outs.operands @
  (operation_operands blk.data) @
  (operation_operands blk.ctrl)

let test_sol_apply_ex1 _ =
  assert_equal ~cmp:Var.Set.equal (all_temps vir1) (all_temps new_vir1);
  assert_equal ~cmp:Var.Set.equal (all_operands vir1) (all_operands new_vir1);
  let blk1 = List.hd_exn vir1.blks in
  let blk2 = List.hd_exn new_vir1.blks in
  assert_equal ~cmp:(List.equal (fun (o1 : operation) o2 -> Tid.equal o1.id o2.id))
    blk1.data (List.rev blk2.data);
  assert_bool "All registers assigned to R0"
    (List.for_all (all_operands_helper blk2)
       ~f:(fun o -> match (op_var_exn o).pre_assign with
           | Some `R0 -> true
           | _ -> false));
  ()

*)

let test_minizinc_ex1 _ =
  let computation =
    (* Set up the KB. *)
    KB.Object.create Data.cls >>= fun obj ->
    Patches.get_bir "ret-3" 32 >>= fun bil ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_bir patch bil >>= fun () ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun () ->
    (* Now run the compiler. *)
    Minizinc.run_minizinc ex1 >>= fun sol ->
    let get_ops ir = let blk = List.hd_exn ir.blks in
      blk.data in
    assert_bool "Operations should be in order"
      (List.for_all2_exn
         ~f:(fun o1 o2 -> Tid.equal o1.id o2.id)
         (get_ops sol)
         (get_ops ex1));
    KB.return obj
  in
  let _ = KB.run Data.cls computation KB.empty in ()

let suite = [
  "Test Minizinc.run_minizinc" >:: test_minizinc_ex1;
]
