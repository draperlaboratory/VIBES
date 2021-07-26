open Core_kernel
open Cabs
open Bap_core_theory


type var_map = unit Theory.var String.Map.t


(* Parses a string into a Cabs.file *)
let parse_c_file (input : string) : (Cabs.file, string) result =
  let open Clexer in
  try
    init_lexicon ();
    Ok (Cparser.file initial (Lexing.from_string input))
  with
  (* | Cparser.Error -> Error "Cparser.Error" *)
  | Parsing.Parse_error -> Error "Error"
  | Cabs.BadType -> Error "BadType"
  | Cabs.BadModifier -> Error "BadModifier"
  | Invalid_argument s -> Error s

(* Parses a sequence of C statements (a body) by wrapping it in a
   dummy function, parsing that as a file and then extracting the
   first def from the result *)
let parse_c_patch (input : string) : (Cabs.definition, string) result =
  let (let*) = Result.(>>=) in
  let p = Printf.sprintf "int FRONTC_PARSING_DUMMY_FUNCTION(){\n%s\n}" input in
  let* parse_result = parse_c_file p in
  match parse_result with
  | def::[] -> Ok def
  | _ -> Error "VIBES: Unexpected Form in FrontC Parse"


module Eval(T : Theory.Core) = struct

  open Theory
  open T
  open KB.Let
  open KB.Syntax


  type ('a, 'b) tgt_info = {
    word_sort : 'a Bitv.t Value.sort;
    byte_sort : 'b Bitv.t Value.sort;
    mem_var   : ('a, 'b) Mem.t var
  }

  let mk_tgt_info (tgt : Theory.target) : ('a, 'b) tgt_info =
    let word_sort = Theory.Bitv.define (Target.bits tgt) in
    let byte_sort = Theory.Bitv.define (Target.byte tgt) in
    (* We assume mem_sort is indexed on word size into bytes, since
       it's the most common. Otherwise we need to query [tgt]
       more... *)
    {
      word_sort = word_sort;
      byte_sort = byte_sort;
      mem_var = Target.data tgt
    }


  let bop_to_pure info (op : Cabs.binary_operator) : unit pure -> unit pure -> unit pure =
    let lift_bop op sort_a sort_b (a : unit pure) (b : unit pure) : unit pure =
      let error = "Incorrect argument sort!" in
      let resort sort v =
        Value.resort (fun _ -> Some sort) v
        |> Option.value_exn ~message:error
        |> KB.return
      in
      let* a = a in
      let* b = b in
      let* res = op (resort sort_a a) (resort sort_b b) in
      KB.return @@ Value.forget res
    in
    let lift_bitv op = lift_bop op info.word_sort info.word_sort in
    match op with
    | ADD -> lift_bitv add
    | SUB -> lift_bitv sub
    | MUL -> lift_bitv mul
    | DIV -> lift_bitv div
    | SHL -> lift_bitv lshift
    (* Use arithmetic shift by default *)
    | SHR  -> lift_bitv arshift
    | EQ  -> lift_bitv eq
    | NE  -> lift_bitv neq
    (* FIXME: use unsigned by default? *)
    | LT  -> lift_bitv slt
    | GT  -> lift_bitv sgt
    | LE  -> lift_bitv sle
    | GE  -> lift_bitv sge
    | AND
    | OR
    | MOD
    | BAND
    | BOR
    | XOR
    | ADD_ASSIGN
    | SUB_ASSIGN
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | BAND_ASSIGN
    | BOR_ASSIGN
    | XOR_ASSIGN
    | SHL_ASSIGN
    | SHR_ASSIGN
    | ASSIGN
      -> failwith "bop_to_pure: binary operator unsupported by VIBES"

  let unop_to_pure info (op : Cabs.unary_operator) : unit pure -> unit pure =
    let lift_uop op sort_a (a : unit pure) : unit pure =
      let error = "Incorrect argument sort!" in
      let resort sort v =
        Value.resort (fun _ -> Some sort) v
        |> Option.value_exn ~message:error
        |> KB.return
      in
      let* a = a in
      let* res = op (resort sort_a a) in
      KB.return @@ Value.forget res
    in
    let lift_bitv op = lift_uop op info.word_sort in
    match op with
    | MEMOF ->
      let load : _ bitv -> _ bitv = loadw info.word_sort b0 (var info.mem_var) in
      lift_bitv load
    | MINUS
    | PLUS
    | NOT
    | BNOT
    | ADDROF
    | PREINCR
    | PREDECR
    | POSINCR
    | POSDECR
      -> failwith "unop_to_pure: unary operator unsupported by VIBES"

  let constant_to_pure info (c :Cabs.constant) : unit pure =
    match c with
    | CONST_INT s ->
      let* res = int info.word_sort Bitvec.(!$ s) in
      KB.return @@ Value.forget res
    | CONST_FLOAT _
    | CONST_CHAR _
    | CONST_STRING _
    | CONST_COMPOUND _ ->
      failwith "constant_to_pure: constant unsupported by VIBES"

  let expr_to_pure info (e : Cabs.expression) (var_map : var_map) : unit pure =
    let rec aux e =
      match e with
      | UNARY (op, a) ->
        let* a = aux a in
        unop_to_pure info op !!a
      | BINARY (op, a, b) ->
        let* a = aux a in
        let* b = aux b in
        bop_to_pure info op !!a !!b
      | INDEX (a, i) ->
        (* Some minor hackery here: turn a[i] into *(a + i) *)
        let index_exp = UNARY (MEMOF, BINARY (ADD, a, i)) in
        aux index_exp
      | VARIABLE x ->
        let v = String.Map.find_exn var_map x in
        let* vv = var v in
        !!vv
      | CONSTANT c ->
        constant_to_pure info c
      | _ -> Cprint.print_expression e 0;
        failwith "FrontC produced expression unsupported by VIBES"
    in
    aux e

  let stmt_to_eff info (s : Cabs.statement) var_map : unit eff =
    let empty_data = Effect.empty (Effect.Sort.data "C_NOP") in
    let empty_ctrl = Effect.empty Effect.Sort.fall in
    let* empty_blk = blk Label.null !!empty_data !!empty_ctrl in
    let data d = blk Label.null !!d !!empty_ctrl in
    let ctrl c = blk Label.null !!empty_data !!c in
    let rec aux s : unit eff =
      match s with
      | NOP ->
        KB.return empty_blk
      (* FIXME: handle all "assignment-like" operations in a seperate function *)
      | COMPUTATION (BINARY (ASSIGN, VARIABLE lval, rval)) ->
        let lval = String.Map.find_exn var_map lval in
        let* rval = expr_to_pure info rval var_map in
        let* assign = set lval !!rval in
        data assign
      | COMPUTATION (CALL (CONSTANT(CONST_INT s), [])) ->
        let dst = int info.word_sort Bitvec.(!$ s) in
        let* jmp = jmp dst in
        ctrl jmp
      | IF (c, true_br, false_br) ->
        let c = expr_to_pure info c var_map in
        (* Downcast [c] to Bool.t *)
        let c = KB.map c
            ~f:(fun c ->
                Value.resort (fun _ -> Some Bool.t) c
                |> Option.value_exn)
        in
        let* true_eff = aux true_br in
        let* false_eff = aux false_br in
        let* branch = branch c !!true_eff !!false_eff in
        KB.return branch
      | SEQUENCE (s1,s2) ->
        let* eff1 = aux s1 in
        let* eff2 = aux s2 in
        let* eff = seq !!eff1 !!eff2 in
        KB.return eff
      | GOTO label ->
        let label = Label.for_name label in
        let* goto = KB.(label >>= goto) in
        ctrl goto
      (* FIXME: allow additional var defs? *)
      | BLOCK ([],s) ->
        let* eff = aux s in
        KB.return eff
      | _ ->
        let s_str = Utils.print_c Cprint.print_statement s in
        let err =
          Format.asprintf "stmt_to_eff: statement %s unsupported by VIBES" s_str
        in
        failwith err
    in
    let* eff = aux s in
    KB.return eff



  let add_def info (def : Cabs.definition) (var_map : var_map) : var_map =
    match def with
    (* FIXME: handle bitwidth here? *)
    | DECDEF (_typ, _storage, names) ->
      let decl s =
        let ty = info.word_sort in
        Var.define ty s |> Var.forget
      in
      List.fold names ~init:var_map
        ~f:(fun map name ->
            let (name, _, _, _) = name in
            String.Map.set map ~key:name ~data:(decl name))
    | _ -> failwith "add_def:Expected DECDEF"

  let defs_to_map info (defs : Cabs.definition list) : var_map =
    List.fold defs ~init:String.Map.empty
      ~f:(fun map def -> add_def info def map)

  let body_to_eff info ((defs, stmt) : Cabs.body) : unit eff =
    let var_map = defs_to_map info defs in
    stmt_to_eff info stmt var_map

  let c_patch_to_eff (tgt : target) (patch : Cabs.definition) : unit eff =
    let info = mk_tgt_info tgt in
    match patch with
    | FUNDEF (_, b) -> body_to_eff info b
    | _ -> failwith "c_patch_to_eff: unexpected patch shape; expected a single fundef!"

end
