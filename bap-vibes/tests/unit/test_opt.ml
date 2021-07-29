open !Core_kernel
open Bap.Std
open Bap_vibes
open OUnit2


let indirect_tgt = Bil.int Bitvector.(of_int ~width:32 42)

let cond = Var.create "cond" (Bil.Imm 32)

let blk1 =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create kind in
  Blk.create ~jmps:[jmp] ()

let blk1_1 =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create ~cond:(Bil.Var cond) kind in
  Blk.create ~jmps:[jmp] ()

let blk1_2 =
  let kind = Goto (Indirect indirect_tgt) in
  let mov = Def.create cond (Bil.var cond) in
  let jmp = Jmp.create ~cond:(Bil.Var cond) kind in
  Blk.create ~defs:[mov] ~jmps:[jmp] ()

let blk2 =
  let kind = Goto (Direct (Term.tid blk1)) in
  let jmp = Jmp.create ~cond:(Bil.Var cond) kind in
  Blk.create ~jmps:[jmp] ()

(* Tests that the optimization actually happened *)
let test_success _ =
  let blks = [blk1; blk2] in
  let opts = Bir_opt.apply blks in
  let blk2 = List.find opts ~f:(fun b -> Tid.(Term.tid b = Term.tid blk2)) in
  match Option.map blk2 ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list) with
  | None -> assert_failure "didn't find block!"
  | Some [j] ->
    begin
      match Option.map (Jmp.dst j) ~f:Jmp.resolve with
      | None -> assert_failure "didn't find dest!"
      | Some (Second v) ->
        let tgt = Bap_core_theory.KB.Value.get Exp.slot v in
        assert_bool "Incorrect target" Exp.(tgt = indirect_tgt)
      | Some (First _) -> assert_failure "Incorrect destination!"
    end
  | _ -> assert_failure "Unexpected block shape!"


(* Optimization shouldn't happen because the second jump is conditional *)
let test_failure_branch _ =
  let blks = [blk1_1; blk2] in
  let opts = Bir_opt.apply blks in
  let blk2 = List.find opts ~f:(fun b -> Tid.(Term.tid b = Term.tid blk2)) in
  match Option.map blk2 ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list) with
  | None -> assert_failure "didn't find block!"
  | Some [j] ->
    begin
      match Option.map (Jmp.dst j) ~f:Jmp.resolve with
      | None -> assert_failure "didn't find dest!"
      | Some (Second _) -> assert_failure "Shouldn't have optimized!"
      | Some (First _) -> ()
    end
  | _ -> assert_failure "Unexpected block shape!"

(* Optimization shouldn't happen because the second block has data effects *)
let test_failure_mov _ =
  let blks = [blk1_2; blk2] in
  let opts = Bir_opt.apply blks in
  let blk2 = List.find opts ~f:(fun b -> Tid.(Term.tid b = Term.tid blk2)) in
  match Option.map blk2 ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list) with
  | None -> assert_failure "didn't find block!"
  | Some [j] ->
    begin
      match Option.map (Jmp.dst j) ~f:Jmp.resolve with
      | None -> assert_failure "didn't find dest!"
      | Some (Second _) -> assert_failure "Shouldn't have optimized!"
      | Some (First _) -> ()
    end
  | _ -> assert_failure "Unexpected block shape!"


let suite = [
  "Test success" >:: test_success;
  "Test failure branch" >:: test_failure_branch;
  "Test failure mov" >:: test_failure_mov;
]
