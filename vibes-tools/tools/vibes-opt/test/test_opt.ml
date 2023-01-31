open Core
open Bap.Std
open Bap_core_theory
open OUnit2

let indirect_tgt : exp = Bil.int @@ Word.of_int ~width:32 42
let cond : var = Var.create "cond" @@ Imm 32
let mov1 () : def term = Def.create cond @@ Bil.var cond
let mov2 () : def term = Def.create cond Bil.(lnot @@ var cond)

let blk_redir () : blk term =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create kind in
  Blk.create ~jmps:[jmp] ()

let blk_cond_redir (blk_dir : blk term) : blk term =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create ~cond:(Bil.Var cond) kind in
  let kind2 = Goto (Direct (Term.tid blk_dir)) in
  let jmp2 = Jmp.create kind2 in
  Blk.create ~jmps:[jmp; jmp2] ()

let blk_mov_redir (mov1 : def term) : blk term =
  let kind = Goto (Indirect indirect_tgt) in
  let jmp = Jmp.create kind in
  Blk.create ~defs:[mov1] ~jmps:[jmp] ()

let blk_dir
    ?(blk_fall : blk term option = None)
    (blk_redir : blk term) : blk term =
  let kind = Goto (Direct (Term.tid blk_redir)) in
  let jmp = Jmp.create ~cond:(Bil.Var cond) kind in
  let jmps = match blk_fall with
    | None -> [jmp]
    | Some b ->
      let kind2 = Goto (Direct (Term.tid b)) in
      let jmp2 = Jmp.create kind2 in
      [jmp; jmp2] in
  Blk.create ~jmps ()

let blk_uncond_dir
    (mov2 : def term)
    (blk_mov_redir : blk term) : blk term =
  let kind = Goto (Direct (Term.tid blk_mov_redir)) in
  let jmp = Jmp.create ~cond:(Bil.Int Word.b1) kind in
  Blk.create ~defs:[mov2] ~jmps:[jmp] ()

let apply (blks : blk term list) : blk term list =
  let state = Toplevel.current () in
  Toplevel.reset ();
  let result = Toplevel.var "bir-opt" in
  let sub = Sub.create () ~blks in
  Toplevel.put result @@ Vibes_opt.Opt.apply [] sub;
  let sub = Toplevel.get result in
  Toplevel.set state;
  Seq.to_list @@ Term.enum blk_t sub

let test_redir_1 (_ : test_ctxt) : unit =
  let redir = blk_redir () in
  let dir = blk_dir redir in
  let opts = apply [dir; redir] in
  List.find opts ~f:(fun b ->
      Tid.equal (Term.tid b) (Term.tid dir)) |>
  Option.map ~f:(fun b ->
      Term.enum jmp_t b |> Seq.to_list) |> function
  | None -> assert_failure "didn't find blk_dir"
  | Some [] | Some (_ :: _ :: _) -> assert_failure "unexpected block shape"
  | Some [j] -> Jmp.dst j |> Option.map ~f:Jmp.resolve |> function
    | None -> assert_failure "didn't find dest"
    | Some (First _) -> assert_failure "should have optimized"
    | Some (Second v) ->
      let tgt = KB.Value.get Exp.slot v in
      assert_bool "incorrect target" Exp.(tgt = indirect_tgt)    

let test_redir_2 (_ : test_ctxt) : unit =
  let redir = blk_redir () in
  let dir = blk_dir redir in
  let cond_redir = blk_cond_redir dir in
  let opts = apply [cond_redir; dir; redir] in
  List.find opts ~f:(fun b ->
      Tid.equal (Term.tid b) (Term.tid dir)) |>
  Option.map ~f:(fun b ->
      Term.enum jmp_t b |> Seq.to_list) |> function
  | None -> assert_failure "didn't find block"
  | Some [] | Some (_ :: _ :: _) -> assert_failure "unexpected block shape"
  | Some [j] -> Jmp.dst j |> Option.map ~f:Jmp.resolve |> function
    | None -> assert_failure "didn't find dest"
    | Some (First _) -> assert_failure "should have optimized"
    | Some (Second v) ->
      let tgt = KB.Value.get Exp.slot v in
      assert_bool "incorrect target" Exp.(tgt = indirect_tgt)

let test_redir_3 (_ : test_ctxt) : unit =
  let mov1 = mov1 () in
  let mov_redir = blk_mov_redir mov1 in
  let redir = blk_redir () in
  let dir = blk_dir redir ~blk_fall:(Some mov_redir) in
  let opts = apply [dir; mov_redir; redir] in
  List.find opts ~f:(fun b ->
      Tid.equal (Term.tid b) (Term.tid dir)) |>
  Option.map ~f:(fun b ->
      Term.enum jmp_t b |> Seq.to_list) |> function
  | None -> assert_failure "didn't find block"
  | Some [] | Some [_] | Some (_ :: _ :: _ :: _) ->
    assert_failure "unexpected block shape"
  | Some [j; _] -> Jmp.dst j |> Option.map ~f:Jmp.resolve |> function
    | None -> assert_failure "didn't find dest"
    | Some (First _) -> assert_failure "should have optimized"
    | Some (Second v) -> 
      let tgt = KB.Value.get Exp.slot v in
      assert_bool "incorrect target" Exp.(tgt = indirect_tgt)

let test_merge (_ : test_ctxt) : unit =
  let mov1 = mov1 () in
  let mov2 = mov2 () in
  let mov_redir = blk_mov_redir mov1 in
  let uncond_dir = blk_uncond_dir mov2 mov_redir in
  let opts = apply [uncond_dir; mov_redir] in
  match opts with
  | [blk] ->
    let tid = Term.tid blk in
    let tid_expected = Term.tid uncond_dir in
    assert_bool
      (sprintf "expected result to be blk %s, got %s"
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
              (sprintf "expected result of blk %s to be %s, got %s"
                 (Tid.to_string tid)
                 (Exp.to_string indirect_tgt)
                 (Exp.to_string e))
              Exp.(e = indirect_tgt)
          | Goto (Direct _) ->
            assert_failure "expected indirect jmp, got direct"
          | _ -> assert_failure "expected goto"
        end
      | _ -> assert_failure "expected singleton jmp"
    end
  | _ -> assert_failure "expected singleton block as result"

let suite : test = "Test BIR optimizer" >::: [
    "Test redir 1" >:: test_redir_1;
    "Test redir 2" >:: test_redir_2;
    "Test redir 3" >:: test_redir_3;
    "Test merge" >:: test_merge;
  ]

let () = match Bap_main.init () with
  | Ok () -> run_test_tt_main suite
  | Error e ->
    Format.eprintf "Failed to initialize BAP: %a"
      Bap_main.Extension.Error.pp e;
    exit 1
