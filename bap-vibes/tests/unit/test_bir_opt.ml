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

open !Core_kernel
open Bap.Std
open Bap_vibes
open Bap_core_theory
open OUnit2

open KB.Let

module Dummy_kb = struct

  type cls
  type t = cls KB.obj
  type computed = (cls, unit) KB.cls KB.value
  let package = "vibes"
  let name = "bir-opt-dummy"
  let cls : (cls, unit) KB.cls = KB.Class.declare ~package name ()

  let opt = KB.Class.property cls ~package:"vibes" "bir-opt" @@
    KB.Domain.optional "bir-opt-domain" ~equal:(List.equal Blk.equal)
end

let indirect_tgt = Bil.int Bitvector.(of_int ~width:32 42)

let cond = Var.create "cond" (Bil.Imm 32)
let mov1 = Def.create cond (Bil.var cond)
let mov2 = Def.create cond Bil.(lnot @@ var cond)

let blk_redir =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create kind in
  Blk.create ~jmps:[jmp] ()

let blk_cond_redir =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create ~cond:(Bil.Var cond) kind in
  Blk.create ~jmps:[jmp] ()

let blk_mov_redir =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create kind in
  Blk.create ~defs:[mov1] ~jmps:[jmp] ()

let blk_dir =
  let kind = Goto (Direct (Term.tid blk_redir)) in
  let jmp = Jmp.create ~cond:(Bil.Var cond) kind in
  Blk.create ~jmps:[jmp] ()

let blk_uncond_dir =
  let kind = Goto (Direct (Term.tid blk_mov_redir)) in
  let jmp = Jmp.create ~cond:(Bil.Int Word.b1) kind in
  Blk.create ~defs:[mov2] ~jmps:[jmp] ()

let apply blks =
  let computation =
    let* obj = KB.Object.create Dummy_kb.cls in
    let* opts = Bir_passes.Opt.apply blks in
    let* () = KB.provide Dummy_kb.opt obj @@ Some opts in
    KB.return obj in
  match KB.run Dummy_kb.cls computation @@ Toplevel.current () with
  | Ok (opts, _) -> begin
      match KB.Value.get Dummy_kb.opt opts with
      | Some opts -> opts
      | None -> failwith "Failed to run the optimizer"
    end
  | Error err -> failwith @@ KB.Conflict.to_string err

(* Tests that the optimization actually happened *)
let test_success _ =
  let blks = [blk_dir; blk_redir] in
  let opts = apply blks in
  let blk_dir = List.find opts ~f:(fun b -> Tid.(Term.tid b = Term.tid blk_dir)) in
  match Option.map blk_dir ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list) with
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
  let blks = [blk_cond_redir; blk_dir] in
  let opts = apply blks in
  let blk_dir = List.find opts ~f:(fun b -> Tid.(Term.tid b = Term.tid blk_dir)) in
  match Option.map blk_dir ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list) with
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
  let blks = [blk_mov_redir; blk_dir] in
  let opts = apply blks in
  let blk_dir = List.find opts ~f:(fun b -> Tid.(Term.tid b = Term.tid blk_dir)) in
  match Option.map blk_dir ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list) with
  | None -> assert_failure "didn't find block!"
  | Some [j] ->
    begin
      match Option.map (Jmp.dst j) ~f:Jmp.resolve with
      | None -> assert_failure "didn't find dest!"
      | Some (Second _) -> assert_failure "Shouldn't have optimized!"
      | Some (First _) -> ()
    end
  | _ -> assert_failure "Unexpected block shape!"

let test_merge _ =
  let blks = [blk_uncond_dir; blk_mov_redir] in
  let opts = apply blks in
  match opts with
  | [blk] ->
    let tid = Term.tid blk in
    let tid_expected = Term.tid blk_uncond_dir in
    assert_bool
      (sprintf "Expected result to be blk %s, got %s"
         (Tid.to_string tid)
         (Tid.to_string tid_expected))
      Tid.(tid = tid_expected);
    let defs = Term.enum def_t blk |> Seq.to_list in
    let expected = [mov2; mov1] in
    assert_bool
      (sprintf "Expected defs %s, got %s"
         (List.to_string ~f:Def.to_string expected)
         (List.to_string ~f:Def.to_string defs))
      (List.equal Def.equal defs expected);
    begin match Term.enum jmp_t blk |> Seq.to_list with
      | [jmp] -> begin
          match Jmp.kind jmp with
          | Goto (Indirect e) ->
            assert_bool
              (sprintf "Expected result of blk %s to be %s, got %s"
                 (Tid.to_string tid)
                 (Exp.to_string indirect_tgt)
                 (Exp.to_string e))
              Exp.(e = indirect_tgt)
          | Goto (Direct _) -> assert_failure "Expected indirect jmp, got direct"
          | _ -> assert_failure "Expected Goto"
        end
      | _ -> assert_failure "Expected singleton jmp"
    end
  | _ -> assert_failure "Expected singleton block as result"

let suite = [
  "Test success" >:: test_success;
  "Test failure branch" >:: test_failure_branch;
  "Test failure mov" >:: test_failure_mov;
  "Test merge" >:: test_merge;
]
