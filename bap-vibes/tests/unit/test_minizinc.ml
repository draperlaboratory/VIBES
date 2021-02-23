open !Core_kernel
open Bap.Std
open Bap_vibes
open OUnit2

open Ir




let ex1 : Ir.t = Test_ir.vir1


let (mzn_params1 , serial_info1) =  Minizinc.serialize_mzn_params ex1 []

let mzn_params_string = Format.asprintf "%a"
    (Yojson.Safe.pretty_print ~std:true)
    (Minizinc.mzn_params_serial_to_yojson mzn_params1)


let mzn_params_gold =
"{
  \"reg_t\": {
    \"set\": [
      {
        \"e\":
          \"((name(Reg(name R0)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R1)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R10)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R11)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R12)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R2)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R3)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R4)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R5)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R6)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R7)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R8)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      },
      {
        \"e\":
          \"((name(Reg(name R9)(ver 0)))(sort(App(args((Int 32)(Sym core-theory:BitVec)))(name(core-theory:BitVec)))))\"
      }
    ]
  },
  \"opcode_t\": { \"set\": [ { \"e\": \"arm:mov\" } ] },
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
      { \"e\": \"#34\" },
      { \"e\": \"#35\" },
      { \"e\": \"#36\" },
      { \"e\": \"#37\" },
      { \"e\": \"#38\" },
      { \"e\": \"#39\" }
    ]
  },
  \"operation_t\": {
    \"set\": [
      { \"e\": \"%0000000d\" },
      { \"e\": \"%0000000e\" },
      { \"e\": \"%0000000f\" },
      { \"e\": \"%00000010\" }
    ]
  },
  \"block_t\": { \"set\": [ { \"e\": \"%00000011\" } ] },
  \"class_t\": { \"set\": [ { \"e\": \"unimplemented_class\" } ] },
  \"operand_operation\": [
    { \"e\": \"%0000000d\" },
    { \"e\": \"%0000000d\" },
    { \"e\": \"%0000000e\" },
    { \"e\": \"%0000000e\" },
    { \"e\": \"%0000000f\" },
    { \"e\": \"%00000010\" }
  ],
  \"definer\": [ { \"e\": \"#39\" }, { \"e\": \"#35\" }, { \"e\": \"#37\" } ],
  \"users\": [
    { \"set\": [ { \"e\": \"#34\" } ] },
    { \"set\": [ { \"e\": \"#36\" } ] },
    { \"set\": [ { \"e\": \"#38\" } ] }
  ],
  \"temp_block\": [
    { \"e\": \"%00000011\" },
    { \"e\": \"%00000011\" },
    { \"e\": \"%00000011\" }
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
  \"operation_opcodes\": [
    { \"set\": [ { \"e\": \"arm:mov\" } ] },
    { \"set\": [ { \"e\": \"arm:mov\" } ] },
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

let dummy_sol : Minizinc.sol =
  {
    reg = Var.Map.empty ;
    opcode = Tid.Map.empty;
    temp = Var.Map.empty;
    active = Tid.Map.empty;
    issue  = Tid.Map.empty;
  }

let sol1 : Minizinc.sol =
  let r0 = Var.create ~is_virtual:false ~fresh:false "r0" (Type.Imm 32) in
  let mov = Ir.Opcode.create ~arch:"arm" "mov" in
  {
    reg = Var.Map.of_alist_exn (List.zip_exn temps1 [r0; r0; r0]) ;
    opcode = Tid.Map.of_alist_exn (List.zip_exn operations1 [mov; mov; mov; mov]);
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
  let r0 = Var.create ~is_virtual:false ~fresh:false "r0" (Type.Imm 32) in
  assert_equal ~cmp:(List.equal (fun (o1 : operation) o2 -> Tid.equal o1.id o2.id))
    blk1.data (List.rev blk2.data);
  assert_bool "All registers assigned to R0"
    (List.for_all (all_operands_helper blk2)
       ~f:(fun o -> Var.(Option.value_exn (op_var_exn o).pre_assign = r0)))

let suite = [
  "Test Definer Serialization" >:: test_definer_serialize_ex1;
  "Test Solution Application" >::  test_sol_apply_ex1;
  (* "Test Minizinc.serialize_mzn_params" >:: test_serialize_gold; *)
]
