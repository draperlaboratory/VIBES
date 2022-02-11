open Core_kernel
open Bap.Std
open Monads.Std
open Bap_core_theory

module Err = Kb_error
module Hvar = Higher_var

type nonrec size = size

type sign = SIGNED | UNSIGNED

type typ =
  | INT of size * sign
  | PTR of typ

type binop =
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LAND
  | LOR
  | XOR
  | SHL
  | SHR
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE

type unop =
  | MINUS
  | NOT
  | LNOT
  | MEMOF
  | ADDROF

type tenv = typ String.Map.t

type var = string * typ

type exp =
  | UNARY of unop * exp * typ
  | BINARY of binop * exp * exp * typ
  | CAST of typ * exp
  | CONST_INT of Bitvec.t * size * sign
  | VARIABLE of var

and stmt =
  | NOP
  | BLOCK of body
  | ASSIGN of var * exp
  | CALL of exp * exp list
  | CALLASSIGN of var * exp * exp list
  | STORE of exp * exp
  | SEQUENCE of stmt * stmt
  | IF of exp * stmt * stmt
  | GOTO of string

and body = tenv * stmt

type t = body

(* Translate back to FrontC representation so we can re-use their
   pretty-printers. *)

let cabs_of_unop : unop -> Cabs.unary_operator = function
  | MINUS -> Cabs.MINUS
  | NOT -> Cabs.NOT
  | LNOT -> Cabs.BNOT
  | MEMOF -> Cabs.MEMOF
  | ADDROF -> Cabs.ADDROF

let cabs_of_binop : binop -> Cabs.binary_operator = function
  | ADD -> Cabs.ADD
  | SUB -> Cabs.SUB
  | MUL -> Cabs.MUL
  | DIV -> Cabs.DIV
  | MOD -> Cabs.MOD
  | LAND -> Cabs.BAND
  | LOR -> Cabs.BOR
  | XOR -> Cabs.XOR
  | SHL -> Cabs.SHL
  | SHR -> Cabs.SHR
  | EQ -> Cabs.EQ
  | NE -> Cabs.NE
  | LT -> Cabs.LT
  | GT -> Cabs.GT
  | LE -> Cabs.LE
  | GE -> Cabs.GE

let rec cabs_of_typ : typ -> Cabs.base_type = function
  | INT (`r8, SIGNED) -> Cabs.(CHAR SIGNED)
  | INT (`r8, UNSIGNED) -> Cabs.(CHAR UNSIGNED)
  | INT (`r16, SIGNED) -> Cabs.(INT (SHORT, SIGNED))
  | INT (`r16, UNSIGNED) -> Cabs.(INT (SHORT, UNSIGNED))
  | INT (`r32, SIGNED) -> Cabs.(INT (LONG, SIGNED))
  | INT (`r32, UNSIGNED) -> Cabs.(INT (LONG, UNSIGNED))
  | INT (`r64, SIGNED) -> Cabs.(INT (LONG_LONG, SIGNED))
  | INT (`r64, UNSIGNED) -> Cabs.(INT (LONG_LONG, UNSIGNED))
  | INT _ -> assert false
  | PTR t -> Cabs.PTR (cabs_of_typ t)

let rec cabs_of_exp : exp -> Cabs.expression = function
  | UNARY (u, e, _) -> Cabs.(UNARY (cabs_of_unop u, cabs_of_exp e))
  | BINARY (b, e1, e2, _) ->
    Cabs.(BINARY (cabs_of_binop b, cabs_of_exp e1, cabs_of_exp e2))
  | CAST (t, e) -> Cabs.(CAST (cabs_of_typ t, cabs_of_exp e))
  | CONST_INT (i, _, _) -> Cabs.(CONSTANT (CONST_INT (Bitvec.to_string i)))
  | VARIABLE (v, _) -> Cabs.VARIABLE v

and cabs_of_stmt : stmt -> Cabs.statement = function
  | NOP -> Cabs.NOP
  | BLOCK (_, s) -> Cabs.BLOCK ([], cabs_of_stmt s)
  | ASSIGN ((v, _), e) ->
    Cabs.(COMPUTATION (BINARY (ASSIGN, VARIABLE v, cabs_of_exp e)))
  | CALL (f, args) ->
    Cabs.(
      COMPUTATION (
        CALL (cabs_of_exp f, List.map args ~f:cabs_of_exp)))
  | CALLASSIGN ((v, _), f, args) ->
    Cabs.(
      COMPUTATION (
        BINARY (
          ASSIGN,
          VARIABLE v,
          CALL (
            cabs_of_exp f,
            List.map args ~f:cabs_of_exp))))
  | STORE (e1, e2) ->
    Cabs.(
      COMPUTATION (
        BINARY (
          ASSIGN,
          UNARY (MEMOF, cabs_of_exp e1),
          cabs_of_exp e2)))
  | SEQUENCE (s1, s2) -> Cabs.SEQUENCE (cabs_of_stmt s1, cabs_of_stmt s2)
  | IF (cond, st, sf) ->
    Cabs.IF (cabs_of_exp cond, cabs_of_stmt st, cabs_of_stmt sf)
  | GOTO lbl -> Cabs.GOTO lbl

let to_string ((_, s) : t) : string =
  Utils.print_c Cprint.print_statement @@ cabs_of_stmt s

(* Extract the embedded type of an expression. *)

let typeof : exp -> typ = function
  | UNARY (_, _, t) -> t
  | BINARY (_, _, _, t) -> t
  | CAST (t, _) -> t
  | CONST_INT (_, size, sign) -> INT (size, sign)
  | VARIABLE (_, t) -> t

(* State monad for elaboration and type-checking. *)

module Transl = struct

  module Env = struct

    type t = {
      target : Theory.target;
      next_temp : int;
      tenv : tenv;
    }

    let create ~(target : Theory.target) () =
      {target; next_temp = 0; tenv = String.Map.empty}

    let typeof (var : string) (env : t) : typ option =
      Map.find env.tenv var

  end

  include Monad.State.T1(Env)(KB)
  include Monad.State.Make(Env)(KB)

  let fail (err : Err.t) : 'a t = lift @@ Err.fail err

end

open Transl.Let

type 'a transl = 'a Transl.t

(* Create a fresh temporary variable. *)
let new_tmp (t : typ) : var transl =
  let* env = Transl.get () in
  let n = env.next_temp in
  let v = sprintf "$%d" n in
  let+ () = Transl.put {
      env with
      next_temp = n + 1;
      tenv = Map.set env.tenv ~key:v ~data:t;
    } in
  v, t

(* A bit of a hack *)
let is_temp (v : string) : bool =
  String.is_prefix v "$" &&
  match Int.of_string @@ String.subo v ~pos:1 with
  | exception _ -> false
  | _ -> true

(* Translate a base type. *)
let rec translate_type (t : Cabs.base_type) : typ transl = match t with
  | Cabs.BOOL -> Transl.return @@ INT (`r8, UNSIGNED)
  | Cabs.CHAR sign -> begin
      match sign with
      | Cabs.(NO_SIGN | SIGNED) -> Transl.return @@ INT (`r8, SIGNED)
      | Cabs.UNSIGNED -> Transl.return @@ INT (`r8, UNSIGNED)
    end
  | Cabs.(INT (size, sign)) ->
    let size = match size with
      | Cabs.(NO_SIZE | LONG) -> `r32
      | Cabs.SHORT -> `r16
      | Cabs.LONG_LONG -> `r64 in
    let sign = match sign with
      | Cabs.(NO_SIGN | SIGNED) -> SIGNED
      | Cabs.UNSIGNED -> UNSIGNED in
    Transl.return @@ INT (size, sign)
  | Cabs.PTR t ->
    let+ t = translate_type t in
    PTR t
  | _ ->
    let s = Utils.print_c (Cprint.print_type ident) t in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_type: unexpected type:\n\n%s" s)

(* Translate a scoped statement. *)
let rec translate_body ((defs, stmt) : Cabs.body) : t transl =
  let* {tenv; _} = Transl.get () in
  let* new_tenv, inits =
    Transl.List.fold defs ~init:(tenv, [])
      ~f:(fun (tenv, inits) -> function
          | DECDEF (_t, _storage, names) ->
            Transl.List.fold names ~init:(tenv, inits)
              ~f:(fun (tenv, inits) (v, t, _, e) ->
                  let+ t = translate_type t in
                  let tenv = Map.set tenv ~key:v ~data:t in
                  tenv, ((v, t), e) :: inits)
          | def ->
            let s = Utils.print_c Cprint.print_def def in
            Transl.fail @@ Core_c_error (
              sprintf "Smallc.translate_body: unexpected definition:\n\n%s\n\n\
                       expected a declaration" s)) in
  let* inits = translate_inits @@ List.rev inits in
  let* () = Transl.update @@ fun env -> {env with tenv = new_tenv} in
  let* s = translate_statement stmt in
  let+ () = Transl.update @@ fun env -> {env with tenv} in
  new_tenv, SEQUENCE (inits, s)

(* Initialize the declared variables. *)
and translate_inits (inits : (var * Cabs.expression) list) : stmt transl =
  Transl.List.fold inits ~init:NOP ~f:(fun acc (v, e) ->
      let+ s, e' = translate_expression e in
      match e' with
      | None -> SEQUENCE (acc, s)
      | Some e' -> SEQUENCE (acc, ASSIGN (v, e')))

(* Translate an expression which may be `None`. *)
and translate_expression ?(computation = false)
    (e : Cabs.expression) : (stmt * exp option) transl =
  let* bits = Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
  let module B = (val Bitvec.modular bits) in
  match e with
  | Cabs.NOTHING -> Transl.return (NOP, None)
  | Cabs.UNARY (u, e) ->
    let+ s, e = translate_unary_operator u e in
    if computation then s, None else s, e
  | Cabs.BINARY (b, lhs, rhs) ->
    let+ s, e = translate_binary_operator b lhs rhs in
    if computation then s, None else s, e
  | Cabs.CAST (t, e) ->
    let* t = translate_type t in
    let+ s, e' =
      translate_expression_strict "translate_expression (CAST)" e in
    if computation then s, None else s, Some (CAST (t, e'))
  | Cabs.CALL (f, args) ->
    let* sf, f' =
      translate_expression_strict "translate_expression (CALL)" f in
    let* sargs, args' =
      (* Evaluated left to right. *)
      Transl.List.fold_right args ~init:([], []) ~f:(fun arg (sargs, args') ->
          let+ s, e =
            translate_expression_strict "translate_expression (CALL)" arg in
          s :: sargs, e :: args') in
    if computation then
      let init = CALL (f', args') in
      let s = List.fold_right (sf :: sargs) ~init ~f:(fun s acc ->
          SEQUENCE (s, acc)) in
      Transl.return (s, None)
    else
      let+ tmp = new_tmp @@ INT (Size.of_int_exn bits, UNSIGNED) in
      let init = CALLASSIGN (tmp, f', args') in
      let s = List.fold_right (sf :: sargs) ~init ~f:(fun s acc ->
          SEQUENCE (s, acc)) in
      s, Some (VARIABLE tmp)
  | Cabs.CONSTANT _ when computation -> Transl.return (NOP, None)
  | Cabs.(CONSTANT (CONST_INT s)) ->
    let i = Int64.of_string s in
    let sign = if Int64.is_negative i then SIGNED else UNSIGNED in
    let size = Size.of_int_exn bits in
    Transl.return (NOP, Some (CONST_INT (B.int64 i, size, sign)))
  | Cabs.(CONSTANT (CONST_CHAR s)) ->
    let i = Char.(to_int @@ of_string s) in
    Transl.return (NOP, Some (CONST_INT (B.int i, `r8, SIGNED)))
  | Cabs.VARIABLE _ when computation -> Transl.return (NOP, None)
  | Cabs.VARIABLE s -> begin
      let* t = Transl.(gets @@ Env.typeof s) in
      match t with
      | Some t -> Transl.return (NOP, Some (VARIABLE (s, t)))
      | None ->
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_expression: undeclared variable %s\n" s)
    end
  | Cabs.EXPR_SIZEOF _ when computation -> Transl.return (NOP, None)
  | Cabs.EXPR_SIZEOF e -> begin
      let+ _, e =
        translate_expression_strict "translate_expression (EXPR_SIZEOF)" e in
      let size = Size.of_int_exn bits in
      match typeof e with
      | INT (size', _) ->
        NOP, Some (CONST_INT (B.int @@ Size.in_bytes size', size, UNSIGNED))
      | PTR _ ->
        NOP, Some (CONST_INT (B.int (bits lsr 3), size, UNSIGNED))
    end
  | _ ->
    let s = Utils.print_c (fun e -> Cprint.print_expression e 0) e in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_expression: unsupported:\n\n%s\n" s)

(* Translate an expression and expect that the value is not `None`. *)
and translate_expression_strict
    (stage : string) (e : Cabs.expression) : (stmt * exp) transl =
  let* s, e' = translate_expression e in
  match e' with
  | Some e' -> Transl.return (s, e')
  | None ->
    let s = Utils.print_c (fun e -> Cprint.print_expression e 0) e in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.%s: invalid expression:\n\n%s\n" stage s)

(* Translate unary operators. *)
and translate_unary_operator
    (u : Cabs.unary_operator)
    (e : Cabs.expression) : (stmt * exp option) transl =
  let exp = translate_expression_strict "translate_unary_operator" in
  match u with
  | Cabs.MINUS ->
    let+ s, e = exp e in
    s, Some (UNARY (MINUS, e, typeof e))
  | Cabs.PLUS ->
    let+ s, e = exp e in
    s, Some e
  | Cabs.NOT ->
    let+ s, e = exp e in
    s, Some (UNARY (NOT, e, typeof e))
  | Cabs.BNOT ->
    let+ s, e = exp e in
    s, Some (UNARY (LNOT, e, typeof e))
  | Cabs.MEMOF -> begin
      let* s, e' = exp e in
      match typeof e' with
      | PTR t -> Transl.return (s, Some (UNARY (MEMOF, e', t)))
      | _ ->
        let s =
          Utils.print_c (fun e -> Cprint.print_expression e 0)
            Cabs.(UNARY (u, e)) in
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_unary_operator: expected pointer type \
                   for expression:\n\n%s\n" s)
    end
  | Cabs.ADDROF ->
    let+ s, e = exp e in
    s, Some (UNARY (ADDROF, e, PTR (typeof e)))
  | Cabs.PREINCR -> translate_increment e ~pre:true ~neg:false 
  | Cabs.POSINCR -> translate_increment e ~pre:false ~neg:false 
  | Cabs.PREDECR -> translate_increment e ~pre:true ~neg:true
  | Cabs.POSDECR -> translate_increment e ~pre:false ~neg:true 

(* Translate the unary increment operators, which are effectful. *)
and translate_increment
    (e : Cabs.expression)
    ~(pre : bool)
    ~(neg : bool) : (stmt * exp option) transl =
  let exp = translate_expression_strict "translate_increment" in
  let* s, e' = exp e in
  let* bits = Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
  let module B = (val Bitvec.modular bits) in
  let t = typeof e' in
  (* Based on the type, get the increment value. *)
  let inc = match t with
    | PTR (INT (size, _)) ->
      (* Pointer to some element type, use the element size. *)
      CONST_INT (B.int @@ Size.in_bytes size, Size.of_int_exn bits, UNSIGNED)
    | PTR (PTR _) ->
      (* Pointer to a pointer: use the word size. *)
      CONST_INT (B.int (bits lsr 3), Size.of_int_exn bits, UNSIGNED)
    | INT (size, sign) ->
      (* Regular integer type, use an increment of one. *)
      CONST_INT (B.one, size, sign) in
  (* Add or subtract based on the increment type. *)
  let op = if neg then SUB else ADD in
  (* If it's a pre-increment, then we return the value after incrementing,
     otherwise we increment the value and return the previous value in a
     temporary var. *)
  let+ s', e' = match e' with
    | VARIABLE var when not pre ->
      let+ tmp = new_tmp t in
      SEQUENCE (
        ASSIGN (tmp, e'),
        ASSIGN (var, BINARY (op, e', inc, t))),
      VARIABLE tmp
    | VARIABLE var ->
      Transl.return (ASSIGN (var, BINARY (op, e', inc, t)), e')
    | UNARY (MEMOF, addr, _) when not pre ->
      let+ tmp = new_tmp t in
      SEQUENCE (
        ASSIGN (tmp, e'),
        STORE (addr, BINARY (op, e', inc, t))),
      VARIABLE tmp
    | UNARY (MEMOF, addr, _) ->
      Transl.return (STORE (addr, BINARY (op, e', inc, t)), e')
    | _ ->
      (* Must be a valid l-value. *)
      let inc_s = if pre then "pre" else "post" in
      let u = match pre, neg with
        | true, false -> Cabs.PREINCR
        | false, false -> Cabs.POSINCR
        | true, true -> Cabs.PREDECR
        | false, true -> Cabs.POSDECR in
      let s =
        Utils.print_c (fun e -> Cprint.print_expression e 0)
          Cabs.(UNARY (u, e)) in
      Transl.fail @@ Core_c_error (
        sprintf "Smallc.translate_increment: %s-increment on \
                 non-lvalue:\n\n%s\n" inc_s s) in
  SEQUENCE (s, s'), Some e'

(* Translate binary operators. *)
and translate_binary_operator
    (b : Cabs.binary_operator)
    (lhs : Cabs.expression)
    (rhs : Cabs.expression) : (stmt * exp option) transl =
  let exp = translate_expression_strict "translate_binary_operator" in
  let default b =
    (* TODO: type check or do implicit conversions here *)
    let* s1, e1 = exp lhs in
    let t1 = typeof e1 in
    let* s2, e2 = exp rhs in
    let t2 = typeof e2 in
    let* tmp1 = new_tmp t1 in
    let+ tmp2 = new_tmp t2 in
    SEQUENCE (
      s1,
      SEQUENCE (
        ASSIGN (tmp1, e1),
        SEQUENCE (
          s2, ASSIGN (tmp2, e2)))),
    Some (BINARY (b, VARIABLE tmp1, VARIABLE tmp2, t1)) in
  match b with
  | Cabs.ADD -> default ADD
  | Cabs.SUB -> default SUB
  | Cabs.MUL -> default MUL
  | Cabs.DIV -> default DIV
  | Cabs.MOD -> default MOD
  | Cabs.AND ->
    (* Short-circuiting boolean AND *)
    let* s1, e1 = exp lhs in
    let t1 = typeof e1 in
    let* s2, e2 = exp rhs in
    let _t2 = typeof e2 in
    let* tmp = new_tmp t1 in
    let+ bits = Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
    let module B = (val Bitvec.modular bits) in
    let size = Size.of_int_exn bits in
    SEQUENCE (
      s1, IF (e1, SEQUENCE (
          s2, ASSIGN (tmp, e2)),
              ASSIGN (tmp, CONST_INT (B.zero, size, UNSIGNED)))),
    Some (VARIABLE tmp)
  | Cabs.OR ->
    (* Short-circuiting boolean OR *)
    let* s1, e1 = exp lhs in
    let t1 = typeof e1 in
    let* s2, e2 = exp rhs in
    let _t2 = typeof e2 in
    let+ tmp = new_tmp t1 in
    SEQUENCE (
      s1, SEQUENCE (
        ASSIGN (tmp, e1),
        IF (VARIABLE tmp, NOP, SEQUENCE (
            s2, ASSIGN (tmp, e2))))),
    Some (VARIABLE tmp)
  | Cabs.BAND -> default LAND
  | Cabs.BOR -> default LOR
  | Cabs.XOR -> default XOR
  | Cabs.SHL -> default SHL
  | Cabs.SHR -> default SHR
  | Cabs.EQ -> default EQ
  | Cabs.NE -> default NE
  | Cabs.GT -> default GT
  | Cabs.LE -> default LE
  | Cabs.GE -> default GE
  | Cabs.ASSIGN ->
    let* s1, e1 = exp lhs in
    let _t1 = typeof e1 in
    let* s2, e2 = exp rhs in
    let _t2 = typeof e2 in
    let+ s = match e1 with
      | VARIABLE var -> Transl.return @@ ASSIGN (var, e2)
      | UNARY (MEMOF, addr, _) -> Transl.return @@ STORE (addr, e2)
      | _ ->
        let s = Utils.print_c (fun e -> Cprint.print_expression e 0) lhs in
        Transl.fail @@ Core_c_error (
          sprintf "Csmall.translate_binary_operator: expected an l-value \
                   for LHS of assignment, got:\n\n%s\n" s) in
    (* Order of evaluation is right-to-left. *)
    SEQUENCE (s2, SEQUENCE (s1, s)), Some e1
  | _ -> Transl.fail @@ Other "unimpl"

(* Translate a statement. *)
and translate_statement (s : Cabs.statement) : stmt transl = match s with
  | Cabs.NOP -> Transl.return NOP
  | Cabs.COMPUTATION e -> begin
      let* s, e = translate_expression e ~computation:true in
      match e with
      | None -> Transl.return s
      | Some e ->
        let+ v = new_tmp @@ typeof e in
        SEQUENCE (s, ASSIGN (v, e))
    end
  | Cabs.BLOCK body ->
    let+ body = translate_body body in
    BLOCK body
  | Cabs.SEQUENCE (s1, s2) ->
    let* s1 = translate_statement s1 in
    let+ s2 = translate_statement s2 in
    SEQUENCE (s1, s2)
  | Cabs.IF (cond, then_, else_) -> begin
      let* s', cond = translate_expression cond in
      match cond with
      | Some cond ->
        let* then_ = translate_statement then_ in
        let+ else_ = translate_statement else_ in
        SEQUENCE (s', IF (cond, then_, else_))
      | None ->
        let s = Utils.print_c Cprint.print_statement s in
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_statement: invalid condition in if \
                   statement:\n\n%s\n" s)
    end
  | Cabs.GOTO lbl -> Transl.return @@ GOTO lbl
  | _ ->
    let s = Utils.print_c Cprint.print_statement s in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_statement: unsupported:\n\n%s\n" s)

(* The elaboration will leave a bunch of nops in the AST which makes
   pretty-printing quite ugly. This pass removes them. *)
let rec cleanup_nop_sequences (s : stmt) : stmt = match s with
  | NOP -> NOP
  | BLOCK (tenv, s) -> BLOCK (tenv, cleanup_nop_sequences s)
  | ASSIGN _ -> s
  | CALL _ -> s
  | CALLASSIGN _ -> s
  | STORE _ -> s
  | SEQUENCE (NOP, s) -> cleanup_nop_sequences s
  | SEQUENCE (s, NOP) -> cleanup_nop_sequences s
  | SEQUENCE (s1, s2) -> begin
      let s1 = cleanup_nop_sequences s1 in
      let s2 = cleanup_nop_sequences s2 in
      match s1, s2 with
      | NOP, _ -> s2
      | _, NOP -> s1
      | _ -> SEQUENCE (s1, s2)
    end
  | IF (cond, st, sf) ->
    IF (cond, cleanup_nop_sequences st, cleanup_nop_sequences sf)
  | GOTO _ -> s

(* Translate a definition. *)
let translate (patch : Cabs.definition) ~(target : Theory.target) : t KB.t =
  let open KB.Let in
  let* body = match patch with
    | FUNDEF (_, b) -> KB.return b
    | _ ->
      let s = Utils.print_c Cprint.print_def patch in
      Err.fail @@ Core_c_error (
        sprintf "Smallc.translate: unexpected patch shape:\n\n%s\n\n\
                 expected a single function definition" s) in
  let+ (tenv, s), _ =
    Transl.Env.create ~target () |> Transl.run (translate_body body) in
  tenv, cleanup_nop_sequences s
