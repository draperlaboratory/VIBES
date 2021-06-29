open !Core_kernel
open Bap_knowledge
open Bap_vibes
open Bap.Std

module KB = Knowledge

open OUnit2

let v1 = Var.create "v1" (Imm 32)
let v2 = Var.create "v2" (Imm 32)
let v3 = Var.create "v3" (Imm 32)
let v = Var.create "v" (Imm 1)
let mem = Var.create "mem" (Mem (`r32, `r32))
let add_goto sub tgt =
  Term.map blk_t sub ~f:(fun blk ->
  let blk = Blk.Builder.init blk in
  Blk.Builder.add_jmp blk @@ Jmp.create @@ Goto (Label.direct tgt);
  Blk.Builder.result blk)

module Prog1 = struct

  let prog =
      let bil =
        Bil.([v1 := var v2 + var v3])
      in
      Bap_wp.Bil_to_bir.bil_to_sub bil

end


module Prog2 = struct

  let prog =
    let bil = Bil.[v1 := var v2 + (var v3 + var v1)] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog3 = struct


  let prog =
    let bil = Bil.[v1 := var v2 lsl var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil


end

module Prog4 = struct

  let prog =
    let bil = Bil.[v1 := var v2 lsr var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog5 = struct


  let prog =
    let bil = Bil.[v1 := var v2 land var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog6 = struct


  let prog =
    let bil = Bil.[v1 := var v2 lor var v3] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end


module Prog9 = struct

  let prog =
    let tgt = Bap.Std.Tid.for_name "tgt" in
    let bil = [] in
    let prog = Bap_wp.Bil_to_bir.bil_to_sub bil in
    add_goto prog tgt

end


module Prog10 = struct


  let prog =
    let bil = Bil.[mem := store ~mem:(var mem) ~addr:(var v1) (var v2) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog11 = struct

  let prog =
    let bil = Bil.[v2 := load ~mem:(var mem) ~addr:(var v1) BigEndian `r32] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Prog12 = struct

  let (!!) i = Bil.int (Word.of_int ~width:32 i)

  let prog =
    let bil = Bil.[if_ (var v) [v1 := !!3] [v1 := !!4]] in
    Bap_wp.Bil_to_bir.bil_to_sub bil

end

module Arm = Arm_selector


let test_ir (_ : test_ctxt) (v : sub term) (expected : string list) : unit =
  let result =
    v |> Term.to_sequence blk_t
    |> Seq.to_list
    |> Arm.ARM_Gen.select
    |> Ir.dummy_reg_alloc
    |> Arm.Pretty.arm_ir_pretty
    |> Result.ok
  in
  let cmp expected input =
    let rexpected =
      Option.map expected
        ~f:(List.map ~f:Str.regexp) in
    begin
      match input, rexpected with
      | Some input, Some expected ->
        let pairs = List.zip expected input in
        begin
          match pairs with
          | Ok pairs ->
            let matches = List.map pairs
                ~f:(fun (pat, str) -> Str.string_match pat str 0) in
            List.for_all ~f:(fun b -> b) matches
          | Unequal_lengths -> false
        end
      | _ -> false
    end
  in
  let print_opt_str_list l =
    match l with
    | None -> "None"
    | Some l -> List.to_string ~f:ident l
  in
  assert_equal
    ~cmp:cmp
    ~printer:print_opt_str_list
    (Some expected) result

(* FIXME: we currently don't check to see if the variable names are
   consistent! *)
let test_ir1 ctxt =
  test_ir ctxt Prog1.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "add R0, R0, R0"; "mov R0, R0"]

let test_ir2 ctxt =
  test_ir ctxt Prog2.prog
    ["blk\\([0-9]\\|[a-f]\\)*:"; "add R0, R0, R0"; "add R0, R0, R0"; "mov R0, R0"]

let test_ir3 ctxt =
  test_ir ctxt Prog3.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "lsl R0, R0, R0"; "mov R0, R0"]

let test_ir4 ctxt =
  test_ir ctxt Prog4.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "lsr R0, R0, R0"; "mov R0, R0"]

let test_ir5 ctxt =
  test_ir ctxt Prog5.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "and R0, R0, R0"; "mov R0, R0"]

let test_ir6 ctxt =
  test_ir ctxt Prog6.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "orr R0, R0, R0"; "mov R0, R0"]

let test_ir9 ctxt =
  test_ir ctxt Prog9.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "b tgt"]

let test_ir10 ctxt =
  test_ir ctxt Prog10.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "str R0, R0"]

let test_ir11 ctxt =
  test_ir ctxt Prog11.prog ["blk\\([0-9]\\|[a-f]\\)*:"; "ldr R0, \\[R0\\]"; "mov R0, R0"]

let test_ir12 ctxt =
  test_ir ctxt Prog12.prog
    [
      "blk\\([0-9]\\|[a-f]\\)*:";
      "cmp R0, #0";
      "beq true_branch";
      "b false_branch";
      "true_branch:";
      "b tgt";
      "false_branch:";
    ]

let suite =
  [
    "Test Arm.ir 1" >:: test_ir1;
    "Test Arm.ir 2" >:: test_ir2;
    "Test Arm.ir 3" >:: test_ir3;
    "Test Arm.ir 4" >:: test_ir4;
    "Test Arm.ir 5" >:: test_ir5;
    "Test Arm.ir 6" >:: test_ir6;
    "Test Arm.ir 9" >:: test_ir9;
    "Test Arm.ir 10" >:: test_ir10;
    "Test Arm.ir 11" >:: test_ir11;
    "Test Arm.ir 12" >:: test_ir12;
  ]
