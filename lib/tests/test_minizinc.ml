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


let test_serialize_ex1 _ = 
  assert_equal (List.length mzn_params1.temp_t.set) 3;
  assert_equal (List.length mzn_params1.operation_t.set) 4;
  assert_equal (List.length mzn_params1.block_t.set) 1;
  ()

let test_definer_serialize_ex1 _ = assert_equal  ~cmp:(Var.Map.equal String.equal) 
    ( Var.Map.map ~f:(fun o -> Var.to_string o.id) Test_vibes_ir.definer_map1) 
    (List.zip_exn serial_info1.temps (List.map ~f:(fun e -> e.e) 
                                        mzn_params1.definer) |> Var.Map.of_alist_exn)

open Test_vibes_ir

let sol1 : Minizinc.sol = {
  reg = Var.Map.of_alist_exn (List.zip_exn  temps1 [  `R0 ; `R0  ; `R0 ]) ;
  insn = Tid.Map.of_alist_exn (List.zip_exn   operations1   [ `MOVi ;`MOVi ;`MOVi ; `MOVi]);
  temp = Var.Map.of_alist_exn (List.zip_exn  operands1  oprnd_temps1  ) ;
  active = Tid.Map.of_alist_exn (List.zip_exn  operations1 [true; true ; true; true])   ;
  issue  = Tid.Map.of_alist_exn (List.zip_exn operations1 [ 4;3;2;1] )   ; (* Just reverse ordered *)
}

let new_vir1 = Minizinc.apply_sol vir1 sol1

(* This is a duplicate of an unexposed function in Vibes_ir *)
let all_operands_helper (blk : blk) : operand list =
  let operation_operands =
    List.concat_map blk.operations
      ~f:(fun operation ->
          operation.lhs @ operation.operands)
  in
  blk.ins.lhs @ blk.outs.operands @ operation_operands

let test_sol_apply_ex1 _ = 
  assert_equal ~cmp:Var.Set.equal (all_temps vir1) (all_temps new_vir1);
  assert_equal ~cmp:Var.Set.equal (all_operands vir1) (all_operands new_vir1);
  let blk1 = List.hd_exn vir1.blks in
  let blk2 = List.hd_exn new_vir1.blks in
  assert_equal ~cmp:(List.equal (fun (o1 : operation) o2 -> Tid.equal o1.id o2.id))  
    blk1.operations (List.rev blk2.operations);
  assert_bool "All registers assigned to R0" (List.for_all (all_operands_helper blk2)
                                                ~f:(fun o -> match (op_var_exn o).pre_assign with
                                                    | Some `R0 -> true
                                                    | _ -> false    ));
  ()



let test_minizinc_ex1 _ =  
  let computation =
    (* Set up the KB. *)
    H.obj () >>= fun obj ->
    Patches.get_BIL H.patch 32 >>= fun bil ->
    KB.Object.create Data.Patch.patch >>= fun patch ->
    Data.Patch.set_bil patch bil >>= fun _ ->
    Data.Patched_exe.set_patches obj
      (Data.Patch_set.singleton patch) >>= fun _ ->
    (* Now run the compiler. *)
    Minizinc.run_minizinc ex1 >>= fun sol ->
    let get_ops ir = let blk = List.hd_exn ir.blks in
      blk.operations in
    assert_bool "Operations should be in order" 
      (List.for_all2_exn ~f:(fun o1 o2 -> Tid.equal o1.id o2.id) (get_ops sol) (get_ops ex1));
    KB.return obj
  in
  let _ = KB.run Data.cls computation KB.empty in ()

let suite = [
  "Test Minizinc.run_minizinc" >:: test_minizinc_ex1;
  "Simple Test Serialization" >:: test_serialize_ex1;
  "Test Definer Serialization" >:: test_definer_serialize_ex1;
  "Test Solution Application" >::  test_sol_apply_ex1;
]
