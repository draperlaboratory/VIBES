open Core_kernel
open Bap_vibes
open Bap.Std
open OUnit2

let prefix_from (blk : Blk.t) : string =
  let tid = Term.tid blk in
  let tid_str = Tid.to_string tid in
  String.drop_prefix tid_str 1

let linearize ~prefix:(prefix : string) (var : Var.t) : Var.t =
  let name = Var.name var in
  let typ = Var.typ var in
  let new_name = prefix ^ "_" ^ name in
  let escaped_name =
    String.substr_replace_all new_name ~pattern:"." ~with_:"_"
  in
  Var.create escaped_name typ

let test_orig_name (_ : test_ctxt) : unit =
  let orig = "_01234567_R0" in
  let expected = "R0" in
  let result = Linear_ssa.orig_name orig in 
  let err = Format.sprintf "expected '%s' but got '%s'" expected result in
  let comparison = String.equal result expected in
  assert_bool err comparison

let test_orig_name_with_ssa (_ : test_ctxt) : unit =
  let orig = "_01234567_R0_1" in
  let expected = "R0" in
  let result = Linear_ssa.orig_name orig in 
  let err = Format.sprintf "expected '%s' but got '%s'" expected result in
  let comparison = String.equal result expected in
  assert_bool err comparison

let test_same (_ : test_ctxt) : unit =
  let a = Var.create "_01234567_R0_1" (Bil.Imm 32) in
  let b = Var.create "_76543210_R0" (Bil.Imm 32) in
  let result = Linear_ssa.same a b in
  let err =
    Format.sprintf "Expected '%s' and '%s' to be the 'same'"
      (Var.name a) 
      (Var.name b)
  in
  assert_bool err result

let test_congruent (_ : test_ctxt) : unit =
  let a = Var.create "_01234567_R0" (Bil.Imm 32) in
  let b = Var.create "_76543210_R0" (Bil.Imm 32) in
  let result = Linear_ssa.congruent a b in
  let err =
    Format.sprintf "Expected '%s' and '%s' to be 'congruent'"
      (Var.name a) 
      (Var.name b)
  in
  assert_bool err result

let test_transform (_ : test_ctxt) : unit =
  (* Build a dummy subroutine to transform into linear SSA. *)
  let var_0 = Var.create "R0" (Bil.Imm 32) in
  let value = Bil.int (Word.of_int 3 ~width:32) in
  let def_1 = Def.create var_0 value in
  let def_2 = Def.create var_0 value in
  let blk_1 = Blk.create () ~defs:[def_1] in
  let blk_2 = Blk.create () ~defs:[def_2] in
  let orig_sub = Sub.create () ~name:"foo" ~blks:[blk_1; blk_2] in
  let sub = Sub.ssa orig_sub in
  let result = Linear_ssa.transform sub in
  (* Build the subroutine we expect it to produce. *)
  let tid = Term.tid sub in
  let blks = Seq.to_list (Term.enum blk_t sub) in
  let tids = List.map blks ~f:Term.tid in
  let prefixes = List.map blks ~f:prefix_from in
  let var_1 = linearize
    (Var.with_index var_0 1)
    ~prefix:(List.nth_exn prefixes 0)
  in
  let var_2 = linearize
    (Var.with_index var_0 2)
    ~prefix:(List.nth_exn prefixes 1)
  in
  let def_1 = Def.create var_1 value ~tid:(Term.tid def_1) in
  let def_2 = Def.create var_2 value ~tid:(Term.tid def_2) in
  let blk_1 = Blk.create () ~tid:(List.nth_exn tids 0) ~defs:[def_1] in
  let blk_2 = Blk.create () ~tid:(List.nth_exn tids 1) ~defs:[def_2] in
  let expected = Sub.create () ~name:"foo" ~tid ~blks:[blk_1; blk_2] in
  let sub_1 = Sub.to_string result in
  let sub_2 = Sub.to_string expected in
  let err =
    Format.sprintf
      "failed to linear SSA sub:\nexpected:\n%sgot:\n%s"
      sub_2 sub_1
  in
  let comparison = String.equal sub_1 sub_2 in
  assert_bool err comparison

let suite = [
  "Test Linear_ssa.orig_name" >:: test_orig_name;
  "Test Linear_ssa.orig_name: with SSA version" >:: test_orig_name_with_ssa;
  "Test Linear_ssa.same" >:: test_same;
  "Test Linear_ssa.congruent" >:: test_congruent;
  "Test Linear_ssa.transform" >:: test_transform;
]
