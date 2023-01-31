open Core
open Bap.Std
open Bap_core_theory
open Bap_wp
open OUnit2

module Hvar = Vibes_higher_vars.Higher_var
module Subst = Vibes_higher_vars.Substituter
module Errors = Vibes_higher_vars.Errors
module Naming = Subst.Naming

(* Very lax equality to avoid tid comparison failures *)
let eq_elt (e1 : Blk.elt) (e2 : Blk.elt) : bool =
  match e1, e2 with
  | `Def d1, `Def d2 ->
    let v1, e1 = Def.lhs d1, Def.rhs d1 in
    let v2, e2 = Def.lhs d2, Def.rhs d2 in
    Var.(v1 = v2) && Exp.(e1 = e2)
  | `Jmp j1, `Jmp j2 ->
    begin
      match Jmp.guard j1, Jmp.guard j2 with
      | None, None -> true
      | Some g1, Some g2 ->
        let g1 = KB.Value.get Exp.slot g1 in
        let g2 = KB.Value.get Exp.slot g2 in
        Exp.(g1 = g2)
      | _ -> false
    end
  (* We shouldn't hit any of these *)
  | `Phi _, `Phi _ -> false
  | _ -> false

let eq_blk_list (b1 : blk term list) (b2 : blk term list) : bool =
  let f = Fn.compose Seq.to_list Blk.elts in
  let b1 = List.concat_map b1 ~f in
  let b2 = List.concat_map b2 ~f in
  List.equal eq_elt b1 b2

let eq_sub (s1 : sub term) (s2 : sub term) : bool =
  eq_blk_list
    (Term.enum blk_t s1 |> Seq.to_list)
    (Term.enum blk_t s2 |> Seq.to_list)

let to_bir (code : bil) : sub term =
  Bil_to_bir.bil_to_sub code |> Term.map blk_t ~f:(fun blk ->
      (* Any jmps that occur after an unconditional jmp should
         be dropped from the blk. *)
      let jmps =
        Term.enum jmp_t blk |> Seq.to_list |>
        List.fold_until ~init:[] ~finish:List.rev ~f:(fun acc jmp ->
            let acc = jmp :: acc in
            if Vibes_bir.Helpers.is_unconditional jmp
            then Stop (List.rev acc)
            else Continue acc) in
      let defs = Seq.to_list @@ Term.enum def_t blk in
      let tid = Term.tid blk in
      Blk.create ~tid ~defs ~jmps ())

let do_subst (hvars : Hvar.t list) (code : bil) : sub term =
  let state = Toplevel.current () in
  Toplevel.reset ();
  let result = Toplevel.var "vibes-subst" in
  let sub = to_bir code in
  let subst =  Subst.substitute sub ~hvars ~target:X86_target.amd64 in
  Toplevel.put result subst;
  let sub = Toplevel.get result in
  Toplevel.set state;
  sub

let str_of_sub (sub : sub term) =
  Term.enum blk_t sub |> Seq.to_list |>
  List.to_string ~f:Blk.to_string

(* Verify that a higher var stored in a register is handled correctly. *)
let test_substitute_1 (_ : test_ctxt) : unit =
  let hvars = Hvar.[{
      name = "x";
      value = Registers {
          at_entry = None;
          at_exit = Some "RAX";
          allow_opt = false;
        };
    }] in
  let x = Var.create "x" @@ Imm 64 in
  let rax = Var.create "RAX" @@ Imm 64 in
  let rax_reg = Naming.mark_reg_unsafe rax in
  let num_3 = Word.of_int ~width:64 3 in
  let code = Bil.[
      x := int @@ num_3;
      if_ (var x = int num_3)
        [jmp (var x)]
        [];
    ] in
  let expected = Bil.[
      x := int @@ num_3;
      if_ (var x = int num_3)
        [jmp (var x)]
        [];
      rax_reg := var x;
    ] |> to_bir in
  let result = do_subst hvars code in
  let msg = Format.asprintf
      "Expected '%s' but got '%s'"
      (str_of_sub expected) (str_of_sub result) in
  let comparison = eq_sub result expected in
  assert_bool msg comparison

(* Verify that a higher var stored on the stack is handled correctly. *)
let test_substitute_2 (_ : test_ctxt) : unit =
  let hvars = Hvar.[{
      name = "x";
      value = Memory (Frame ("RBP", Word.of_int 0x14 ~width:64));
    }] in
  let x = Var.create "x" @@ Imm 64 in
  let rbp = Var.create "RBP" @@ Imm 64 in
  let rbp_reg = Naming.mark_reg_unsafe rbp in
  let mem = Var.create "mem" @@ Mem (`r64, `r8) in
  let num_3 = Word.of_int ~width:64 3 in
  let num_14 = Word.of_string "0x14:64" in
  let code = Bil.[
      x := int @@ num_3;
      if_ (var x = int num_3)
        [jmp (var x)]
        [];
    ] in
  let expected = Bil.[
      mem :=
        store
          ~mem:(var mem)
          ~addr:(var rbp_reg + int num_14)
          (int num_3)
          LittleEndian
          `r64;
      if_ (load
             ~mem:(var mem)
             ~addr:(var rbp_reg + int num_14)
             LittleEndian `r64
           = int num_3)
        [jmp (load
                ~mem:(var mem)
                ~addr:(var rbp_reg + int num_14)
                LittleEndian `r64)]
        []
    ] |> to_bir in
  let result = do_subst hvars code in
  let msg = Format.asprintf "Expected '%s' but got '%s'"
      (str_of_sub expected) (str_of_sub result) in
  let comparison = eq_sub result expected in
  assert_bool msg comparison

(* Verify that substitution errors are raised correctly. *)
let test_substitute_error (_ : test_ctxt) : unit =
  let hvars = Hvar.[{
      name = "x";
      value = Registers {
          at_entry = Some "FAKE1";
          at_exit = Some "FAKE2";
          allow_opt = false;
        };
    }] in
  let x = Var.create "x" (Bil.Imm 64) in
  let code = Bil.[x := var x] in
  match do_subst hvars code with
  | exception (Toplevel.Conflict (Errors.Higher_var_not_substituted _)) -> ()
  | _ ->  assert_failure "Expected error"

let suite : test = "Test Substituter" >::: [
    "Test case #1" >:: test_substitute_1;
    "Test case #2" >:: test_substitute_2;
    "Test error case" >:: test_substitute_error;
  ]

let () = match Bap_main.init () with
  | Ok () -> run_test_tt_main suite
  | Error e ->
    Format.eprintf "Failed to initialize BAP: %a"
      Bap_main.Extension.Error.pp e;
    exit 1
