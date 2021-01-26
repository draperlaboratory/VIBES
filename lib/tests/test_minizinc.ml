open !Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_vibes
open OUnit2
module KB = Knowledge
module H = Helpers
open Knowledge.Syntax

open Vibes_ir




let ex1 : Vibes_ir.t = Test_vibes_ir.vir1


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
    (Var.Map.map ~f:(fun o -> Var.to_string o.id) Test_vibes_ir.definer_map1)
    (List.zip_exn serial_info1.temps
       (List.map ~f:(fun e -> e.e)
          mzn_params1.definer) |> Var.Map.of_alist_exn)


open Test_vibes_ir

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



let test_minizinc_ex1 _ =
  let computation =
    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Patches.get_bir H.patch 32 >>= fun bil ->
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
  "Test Definer Serialization" >:: test_definer_serialize_ex1;
  "Test Solution Application" >::  test_sol_apply_ex1;
  "Test Minizinc.serialize_mzn_params" >:: test_serialize_gold;
]
