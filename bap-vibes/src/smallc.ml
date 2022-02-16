open Core_kernel
open Bap.Std
open Monads.Std
open Bap_core_theory

module Err = Kb_error
module Hvar = Higher_var

type nonrec size = size

let equal_size = Size.equal

type sign = SIGNED | UNSIGNED [@@deriving equal]

type typ =
  | INT of size * sign
  | PTR of typ
[@@deriving equal]

let size_of_typ (target : Theory.target) : typ -> int = function
  | INT (size, _) -> Size.in_bits size
  | PTR _ -> Theory.Target.bits target

let sign_of_typ : typ -> sign = function
  | INT (_, s) -> s
  | PTR _ -> UNSIGNED

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
[@@deriving equal]

type unop =
  | MINUS
  | LNOT
  | MEMOF
  | ADDROF
[@@deriving equal]

type tenv = typ String.Map.t [@@deriving equal]

type var = Theory.Var.Top.t * typ [@@deriving equal]

let equal_word = Word.equal

type exp =
  | UNARY of unop * exp * typ
  | BINARY of binop * exp * exp * typ
  | CAST of typ * exp
  | CONST_INT of word * sign
  | VARIABLE of var
[@@deriving equal]

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
[@@deriving equal]

and body = tenv * stmt [@@deriving equal]

type t = body

(* Translate back to FrontC representation so we can re-use their
   pretty-printers. *)

let cabs_of_unop : unop -> Cabs.unary_operator = function
  | MINUS  -> Cabs.MINUS
  | LNOT   -> Cabs.BNOT
  | MEMOF  -> Cabs.MEMOF
  | ADDROF -> Cabs.ADDROF

let cabs_of_binop : binop -> Cabs.binary_operator = function
  | ADD  -> Cabs.ADD
  | SUB  -> Cabs.SUB
  | MUL  -> Cabs.MUL
  | DIV  -> Cabs.DIV
  | MOD  -> Cabs.MOD
  | LAND -> Cabs.BAND
  | LOR  -> Cabs.BOR
  | XOR  -> Cabs.XOR
  | SHL  -> Cabs.SHL
  | SHR  -> Cabs.SHR
  | EQ   -> Cabs.EQ
  | NE   -> Cabs.NE
  | LT   -> Cabs.LT
  | GT   -> Cabs.GT
  | LE   -> Cabs.LE
  | GE   -> Cabs.GE

let rec cabs_of_typ : typ -> Cabs.base_type = function
  | INT (`r8, SIGNED)    -> Cabs.(CHAR SIGNED)
  | INT (`r8, UNSIGNED)  -> Cabs.(CHAR UNSIGNED)
  | INT (`r16, SIGNED)   -> Cabs.(INT (SHORT, SIGNED))
  | INT (`r16, UNSIGNED) -> Cabs.(INT (SHORT, UNSIGNED))
  | INT (`r32, SIGNED)   -> Cabs.(INT (LONG, SIGNED))
  | INT (`r32, UNSIGNED) -> Cabs.(INT (LONG, UNSIGNED))
  | INT (`r64, SIGNED)   -> Cabs.(INT (LONG_LONG, SIGNED))
  | INT (`r64, UNSIGNED) -> Cabs.(INT (LONG_LONG, UNSIGNED))
  | INT _                -> assert false
  | PTR t                -> Cabs.PTR (cabs_of_typ t)

let rec cabs_of_exp : exp -> Cabs.expression = function
  | UNARY (u, e, _) -> Cabs.(UNARY (cabs_of_unop u, cabs_of_exp e))
  | BINARY (b, e1, e2, _) ->
    Cabs.(BINARY (cabs_of_binop b, cabs_of_exp e1, cabs_of_exp e2))
  | CAST (t, e) -> Cabs.(CAST (cabs_of_typ t, cabs_of_exp e))
  | CONST_INT (i, sign) ->
    Cabs.(CONSTANT (CONST_INT (Bitvec.to_string @@ Word.to_bitvec i)))
  | VARIABLE (v, _) -> Cabs.VARIABLE (Theory.Var.name v)

and cabs_of_stmt : stmt -> Cabs.statement = function
  | NOP -> Cabs.NOP
  | BLOCK (_, s) -> Cabs.BLOCK ([], cabs_of_stmt s)
  | ASSIGN ((v, _), e) ->
    Cabs.(
      COMPUTATION (
        BINARY (
          ASSIGN,
          VARIABLE (Theory.Var.name v),
          cabs_of_exp e)))
  | CALL (f, args) ->
    Cabs.(
      COMPUTATION (
        CALL (cabs_of_exp f, List.map args ~f:cabs_of_exp)))
  | CALLASSIGN ((v, _), f, args) ->
    Cabs.(
      COMPUTATION (
        BINARY (
          ASSIGN,
          VARIABLE (Theory.Var.name v),
          CALL (
            cabs_of_exp f,
            List.map args ~f:cabs_of_exp))))
  | STORE (addr, value) ->
    Cabs.(
      COMPUTATION (
        BINARY (
          ASSIGN,
          UNARY (MEMOF, cabs_of_exp addr),
          cabs_of_exp addr)))
  | SEQUENCE (s1, s2) -> Cabs.SEQUENCE (cabs_of_stmt s1, cabs_of_stmt s2)
  | IF (cond, st, sf) ->
    Cabs.IF (cabs_of_exp cond, cabs_of_stmt st, cabs_of_stmt sf)
  | GOTO lbl -> Cabs.GOTO lbl

let to_string ((tenv, s) : t) : string =
  let vars =
    Map.to_alist tenv |> List.map ~f:(fun (v, t) ->
        let s = Utils.print_c (Cprint.print_type ident) (cabs_of_typ t) in
        sprintf "%s %s;" s v) |>
    String.concat ~sep:"\n" in
  let stmt = Utils.print_c Cprint.print_statement @@ cabs_of_stmt s in
  sprintf "%s\n%s" vars stmt

(* Extract the embedded type of an expression. *)

let typeof : exp -> typ = function
  | UNARY (_, _, t) -> t
  | BINARY (_, _, _, t) -> t
  | CAST (t, _) -> t
  | CONST_INT (i, sign) ->
    let w = Word.bitwidth i in
    INT (Size.of_int_exn w, sign)
  | VARIABLE (_, t) -> t

(* Convert to a particular type. *)
let with_type (e : exp) (t : typ) : exp = match e with
  | UNARY _ | BINARY _ | VARIABLE _ -> CAST (t, e)
  | CAST (_, e) -> CAST (t, e)
  | CONST_INT (i, _) -> begin 
      match t with
      | INT (size', sign') ->
        let i = match sign' with
          | SIGNED -> Word.signed i
          | UNSIGNED -> Word.unsigned i in
        let w = Size.in_bits size' in
        let i = Word.extract_exn ~hi:(w - 1) i in
        CONST_INT (i, sign')
      | _ -> e
    end

(* State monad for elaboration and type-checking. *)

module Transl = struct

  module Env = struct

    type t = {
      target : Theory.target;
      tenv : tenv;
    }

    let create ~(target : Theory.target) () =
      {target; tenv = String.Map.empty}

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
  let* {target; _} = Transl.get () in
  let s = Theory.Bitv.define @@ size_of_typ target t in
  let* v = Transl.lift @@ Theory.Var.fresh s in
  let v = Theory.Var.forget v in
  let+ () = Transl.update @@ fun env -> {
      env with tenv = Map.set env.tenv ~key:(Theory.Var.name v) ~data:t;
    } in
  v, t

(* A bit of a hack *)
let is_temp (v : string) : bool =
  String.is_prefix v "$" &&
  match Int.of_string @@ String.subo v ~pos:1 with
  | exception _ -> false
  | _ -> true

(* Translate a base type. *)
let rec translate_type
    ?(msg : string = "")
    (t : Cabs.base_type) : typ transl = match t with
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
      sprintf "Smallc.translate_type: %sunsupported type:\n\n%s" msg s)

(* Perform type conversions for pure expressions. *)
let typ_unify (t1 : typ) (t2 : typ) : typ option =
  match t1, t2 with
  | INT _, PTR _ | PTR _, INT _ -> None
  | PTR t1', PTR t2' -> if equal_typ t1' t2' then Some t1 else None
  | INT (size1, sign1), INT (size2, sign2) ->
    match Size.compare size1 size2 with
    | n when n < 0 -> Some t2
    | n when n > 0 -> Some t1
    | _ -> match sign1, sign2 with
      | SIGNED, UNSIGNED | UNSIGNED, SIGNED ->
        Some (INT (size1, UNSIGNED))
      | _ -> Some t1

(* Perform type conversions for an assignment. Returns the unified type
   and the expression with an explicit cast. *)
let typ_unify_assign (tl : typ) (tr : typ) (r : exp) : (typ * exp) option =
  match tl, tr with
  | INT _, PTR _ | PTR _, INT _ -> None
  | PTR t1', PTR t2' -> if equal_typ t1' t2' then Some (tl, r) else None
  | INT (sizel, signl), INT (sizer, signr) ->
    if equal_size sizel sizer && equal_sign signl signr
    then Some (tl, r) else Some (tl, CAST (tl, r))

(* Translate a scoped statement. *)
let rec translate_body ((defs, stmt) : Cabs.body) : t transl =
  let* {target; tenv} = Transl.get () in
  let* new_tenv, inits =
    Transl.List.fold defs ~init:(tenv, [])
      ~f:(fun (tenv, inits) -> function
          | DECDEF (_t, _storage, names) ->
            Transl.List.fold names ~init:(tenv, inits)
              ~f:(fun (tenv, inits) (v, t, _, e) ->
                  let+ t = translate_type t in
                  let s = Theory.Bitv.define @@ size_of_typ target t in
                  let tenv = Map.set tenv ~key:v ~data:t in
                  let v = Theory.Var.(forget @@ define s v) in
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

and typ_unify_error (e : Cabs.expression) (t1 : typ) (t2 : typ) : 'a transl =
  let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
  let s1 = Utils.print_c (Cprint.print_type ident) (cabs_of_typ t1) in
  let s2 = Utils.print_c (Cprint.print_type ident) (cabs_of_typ t2) in
  Transl.fail @@ Core_c_error (
    sprintf "Failed to unify types %s and %s in expression:\n\n%s\n" s1 s2 s)

and typ_error (e : Cabs.expression) (t : typ) (msg : string) : 'a transl =
  let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
  let t = Utils.print_c (Cprint.print_type ident) (cabs_of_typ t) in
  Transl.fail @@ Core_c_error (
    sprintf "Expression:\n\n%s\n\nunified to type %s. %s\n" s t msg)

(* Translate an expression which may be `None`. Also returns any side effects
   produced by the expression. 

   `assign` denoted whether we want the result of evaluating this expression
   to be assigned to a particular variable. Otherwise, a fresh temporary is
   created to hold the result

   `computation` denotes whether this expression was derived from a FrontC
   COMPUTATION statement. This means that the expression is being evaluated
   for its side effects only, and the result may then be discarded.
*)
and translate_expression
    ?(assign : var option = None)
    ?(computation = false)
    (e : Cabs.expression) : (stmt * exp option) transl =
  match e with
  | Cabs.NOTHING -> Transl.return (NOP, None)
  | Cabs.UNARY (u, e) ->
    let+ s, e = translate_unary_operator u e in
    if computation then s, None else s, e
  | Cabs.BINARY (b, lhs, rhs) ->
    let+ s, e = translate_binary_operator b lhs rhs in
    if computation then s, None else s, e
  | Cabs.QUESTION (cond, then_, else_) -> begin
      let exp = translate_expression_strict "translate_expression (QUESTION)" in
      let* scond, cond = exp cond in
      let* sthen, then_ = exp then_ in
      let* selse, else_ = exp else_ in
      let t1 = typeof then_ in
      let t2 = typeof else_ in
      match typ_unify t1 t2 with
      | None -> typ_unify_error e t1 t2
      | Some t ->
        let then_ = with_type then_ t in
        let else_ = with_type else_ t in
        let+ v = match assign with
          | Some v -> Transl.return v
          | None -> new_tmp t in
        let s =
          SEQUENCE (
            scond,
            IF (
              cond,
              SEQUENCE (sthen, ASSIGN (v, then_)),
              SEQUENCE (selse, ASSIGN (v, else_)))) in
        s, Some (VARIABLE v)
    end
  | Cabs.CAST (t, e) ->
    let* t = translate_type t in
    let+ s, e' =
      translate_expression_strict "translate_expression (CAST)" e ~assign in
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
      (* We don't actually know the return type of the function, so assume
         it's an integer that will fit inside of a machine register. *)
      let* bits = Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
      let+ v = match assign with
        | Some v -> Transl.return v
        | None -> new_tmp @@ INT (Size.of_int_exn bits, UNSIGNED) in
      let init = CALLASSIGN (v, f', args') in
      let s = List.fold_right (sf :: sargs) ~init ~f:(fun s acc ->
          SEQUENCE (s, acc)) in
      s, Some (VARIABLE v)
  | Cabs.CONSTANT _ when computation -> Transl.return (NOP, None)
  | Cabs.(CONSTANT (CONST_INT s)) ->
    let i = Int64.of_string s in
    let sign = if Int64.is_negative i then SIGNED else UNSIGNED in
    let width =
      if Int64.(i <= 0xFFL) then 8
      else if Int64.(i <= 0xFFFFL) then 16
      else if Int64.(i <= 0xFFFFFFFFL) then 32
      else 64 in
    let i = Word.of_int64 ~width i in
    (* `word` is unsigned by default. *)
    let i = if equal_sign sign SIGNED then Word.signed i else i in
    Transl.return (NOP, Some (CONST_INT (i, sign)))
  | Cabs.(CONSTANT (CONST_CHAR s)) ->
    let i = Word.of_int ~width:8 Char.(to_int @@ of_string s) in
    let i = Word.signed i in
    Transl.return (NOP, Some (CONST_INT (i, SIGNED)))
  | Cabs.VARIABLE _ when computation -> Transl.return (NOP, None)
  | Cabs.VARIABLE v -> begin
      let* t = Transl.(gets @@ Env.typeof v) in
      match t with
      | Some t ->
        let+ {target; _} = Transl.get () in
        let s = Theory.Bitv.define @@ size_of_typ target t in
        let v = Theory.Var.define s v |> Theory.Var.forget in
        NOP, Some (VARIABLE (v, t))
      | None ->
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_expression: undeclared variable %s\n" v)
    end
  | Cabs.EXPR_SIZEOF _ when computation -> Transl.return (NOP, None)
  | Cabs.EXPR_SIZEOF e -> begin
      let* _, e =
        translate_expression_strict "translate_expression (EXPR_SIZEOF)" e in
      let+ bits = Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
      match typeof e with
      | INT (size', _) ->
        let i = Word.of_int ~width:bits @@ Size.in_bytes size' in
        NOP, Some (CONST_INT (i, UNSIGNED))
      | PTR _ ->
        let i = Word.of_int ~width:bits @@ bits lsr 3 in
        NOP, Some (CONST_INT (i, UNSIGNED))
    end
  | Cabs.TYPE_SIZEOF t ->
    let s = Utils.print_c Cprint.print_base_type t in
    let* t = translate_type t ~msg:(sprintf "In expression %s: " s) in
    let+ {target; _} = Transl.get () in
    let bits = Theory.Target.bits target in
    let i = Word.of_int ~width:bits @@ size_of_typ target t in
    NOP, Some (CONST_INT (i, UNSIGNED))
  | Cabs.INDEX (ptr, idx) -> begin
      let exp = translate_expression_strict "translate_expression (INDEX)" in
      let* sptr, eptr = exp ptr in
      let* sidx, eidx = exp idx in
      let tptr = typeof eptr in
      let tidx = typeof eidx in
      match tptr, tidx with
      | PTR t, INT _ ->
        (* Translate to the pointer arithmetic of an array lookup. *)
        let+ {target; _} = Transl.get () in
        let bits = Theory.Target.bits target in
        let stride = size_of_typ target t lsr 3 in
        let scale = Word.of_int ~width:bits stride in
        let tidx = INT (Size.of_int_exn bits, UNSIGNED) in
        let eidx = with_type eidx tidx in
        let e =
          UNARY (
            MEMOF,
            BINARY (
              ADD,
              CAST (tidx, eptr),
              BINARY (
                MUL,
                CONST_INT (scale, UNSIGNED),
                eidx,
                tidx),
              tidx),
            t) in
        SEQUENCE (sptr, sidx), Some e
      | PTR _, _ ->
        let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
        let t = Utils.print_c (Cprint.print_type ident) (cabs_of_typ tidx) in
        Transl.fail @@ Core_c_error (
          sprintf "Expression:\n\n%s\n\nIndex operand has type %s. \
                   Expected integer.\n" s t)
      | _, _ ->
        let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
        let t = Utils.print_c (Cprint.print_type ident) (cabs_of_typ tptr) in
        Transl.fail @@ Core_c_error (
          sprintf "Expression:\n\n%s\n\nArray operand has type %s. \
                   Expected pointer.\n" s t)
    end
  | _ ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_expression: unsupported:\n\n%s\n" s)

(* Translate an expression and expect that the value is not `None`. *)
and translate_expression_strict
    ?(assign : var option = None)
    (stage : string)
    (e : Cabs.expression) : (stmt * exp) transl =
  let* s, e' = translate_expression e ~assign in
  match e' with
  | Some e' -> Transl.return (s, e')
  | None ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
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
    let* s, e = exp e in
    let+ {target; _} = Transl.get () in
    let width = size_of_typ target @@ typeof e in
    let i = Word.zero width in
    s, Some (BINARY (NE, e, CONST_INT (i, UNSIGNED),typeof e))
  | Cabs.BNOT ->
    let+ s, e = exp e in
    s, Some (UNARY (LNOT, e, typeof e))
  | Cabs.MEMOF -> begin
      let* s, e' = exp e in
      match typeof e' with
      | PTR t -> Transl.return (s, Some (UNARY (MEMOF, e', t)))
      | _ ->
        let s =
          Utils.print_c Cprint.print_statement
            Cabs.(COMPUTATION (UNARY (u, e))) in
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_unary_operator: expected pointer type \
                   for operand of expression:\n\n%s\n" s)
    end
  | Cabs.ADDROF -> begin
      let* s, e' = exp e in
      match e' with
      | VARIABLE (_, t) | UNARY (MEMOF, _, t) ->
        Transl.return (s, Some (UNARY (ADDROF, e', PTR t)))
      | _ ->
        let s =
          Utils.print_c Cprint.print_statement
            Cabs.(COMPUTATION (UNARY (u, e))) in
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_unary_operator: ADDROF requires lvalue \
                   for operand, got:\n\n%s\n" s)
    end
  | Cabs.PREINCR -> translate_increment e ~pre:true  ~neg:false 
  | Cabs.POSINCR -> translate_increment e ~pre:false ~neg:false 
  | Cabs.PREDECR -> translate_increment e ~pre:true  ~neg:true
  | Cabs.POSDECR -> translate_increment e ~pre:false ~neg:true 

(* Translate the unary increment operators, which are effectful. *)
and translate_increment
    (e : Cabs.expression)
    ~(pre : bool)
    ~(neg : bool) : (stmt * exp option) transl =
  let exp = translate_expression_strict "translate_increment" in
  let* s, e' = exp e in
  let* bits = Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
  let t = typeof e' in
  (* Based on the type, get the increment value. *)
  let inc = match t with
    | PTR (INT (size, _)) ->
      (* Pointer to some element type, use the element size. *)
      let i = Word.of_int ~width:bits @@ Size.in_bytes size in
      CONST_INT (i, UNSIGNED)
    | PTR (PTR _) ->
      (* Pointer to a pointer: use the word size. *)
      let i = Word.of_int ~width:bits @@ bits lsr 3 in
      CONST_INT (i, UNSIGNED)
    | INT (size, sign) ->
      (* Regular integer type, use an increment of one. *)
      let i = Word.one bits in
      let i = if equal_sign sign SIGNED then Word.signed i else i in
      CONST_INT (i, sign) in
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
        | true,  false -> Cabs.PREINCR
        | false, false -> Cabs.POSINCR
        | true,  true  -> Cabs.PREDECR
        | false, true  -> Cabs.POSDECR in
      let s =
        Utils.print_c Cprint.print_statement
          Cabs.(COMPUTATION (UNARY (u, e))) in
      Transl.fail @@ Core_c_error (
        sprintf "Smallc.translate_increment: %s-increment on \
                 non-lvalue:\n\n%s\n" inc_s s) in
  SEQUENCE (s, s'), Some e'

(* Translate binary operators. *)
and translate_binary_operator
    (b : Cabs.binary_operator)
    (lhs : Cabs.expression)
    (rhs : Cabs.expression) : (stmt * exp option) transl =
  let exp ?(assign = None) =
    translate_expression_strict "translate_binary_operator" ~assign in
  let default op =
    (* TODO: type check or do implicit conversions here *)
    let* s1, e1 = exp lhs in
    let t1 = typeof e1 in
    let* s2, e2 = exp rhs in
    let t2 = typeof e2 in
    match typ_unify t1 t2 with
    | None -> typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
    | Some t ->
      let e1 = with_type e1 t in
      let e2 = with_type e2 t in
      let* tmp1 = new_tmp t in
      let+ tmp2 = new_tmp t in
      SEQUENCE (
        s1,
        SEQUENCE (
          ASSIGN (tmp1, e1),
          SEQUENCE (s2, ASSIGN (tmp2, e2)))),
      Some (BINARY (op, VARIABLE tmp1, VARIABLE tmp2, t)) in
  match b with
  | Cabs.ADD -> default ADD
  | Cabs.SUB -> default SUB
  | Cabs.MUL -> default MUL
  | Cabs.DIV -> default DIV
  | Cabs.MOD -> default MOD
  | Cabs.AND -> begin
      (* Short-circuiting boolean AND *)
      let* s1, e1 = exp lhs in
      let t1 = typeof e1 in
      let* s2, e2 = exp rhs in
      let t2 = typeof e2 in
      match typ_unify t1 t2 with
      | None -> typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
      | Some (INT (size, sign) as t) ->
        let e1 = with_type e1 t in
        let e2 = with_type e2 t in
        let+ tmp = new_tmp t in
        SEQUENCE (
          s1,
          SEQUENCE (
            ASSIGN (tmp, e1),
            IF (VARIABLE tmp,
                SEQUENCE (
                  s2,
                  ASSIGN (tmp, e2)),
                NOP))),
        Some (VARIABLE tmp)
      | Some t ->
        typ_error Cabs.(BINARY (b, lhs, rhs)) t
          "Expected an integer type."
    end
  | Cabs.OR -> begin
      (* Short-circuiting boolean OR *)
      let* s1, e1 = exp lhs in
      let t1 = typeof e1 in
      let* s2, e2 = exp rhs in
      let t2 = typeof e2 in
      match typ_unify t1 t2 with
      | None -> typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
      | Some (INT (size, sign) as t) ->
        let e1 = with_type e1 t in
        let e2 = with_type e2 t in
        let+ tmp = new_tmp t in
        SEQUENCE (
          s1, SEQUENCE (
            ASSIGN (tmp, e1),
            IF (VARIABLE tmp, NOP, SEQUENCE (
                s2, ASSIGN (tmp, e2))))),
        Some (VARIABLE tmp)
      | Some t ->
        typ_error Cabs.(BINARY (b, lhs, rhs)) t
          "Expected an integer type."
    end
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
  | Cabs.ASSIGN -> begin
      let* s1, e1 = exp lhs in
      let* s2, e2 = match e1 with
        | VARIABLE v -> exp rhs ~assign:(Some v)
        | _ -> exp rhs in
      let t2 = typeof e2 in
      let* t1, is_store = match e1 with
        | VARIABLE (_, t) -> Transl.return (t, false)
        | UNARY (MEMOF, _, t) -> Transl.return (t, true)
        | _ ->
          let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION lhs) in
          Transl.fail @@ Core_c_error (
            sprintf "Csmall.translate_binary_operator: expected an l-value \
                     for LHS of assignment, got:\n\n%s\n" s) in
      match typ_unify_assign t1 t2 e2 with
      | None ->
        let t1 = if is_store then PTR t1 else t1 in
        typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
      | Some (_, e2) ->
        let s = match e1 with
          | VARIABLE var -> ASSIGN (var, e2)
          | UNARY (MEMOF, addr, _) -> STORE (addr, e2)
          | _ -> assert false in
        (* Order of evaluation is right-to-left. *)
        Transl.return (SEQUENCE (s2, SEQUENCE (s1, s)), Some e1)
    end
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

(* Remove unnecessary casts from expressions. *)
let rec simpl_casts_exp : exp -> exp = function
  | UNARY (u, e, t) -> UNARY (u, simpl_casts t e, t)
  | BINARY (b, l, r, t) ->
    BINARY (b, simpl_casts t l, simpl_casts t r, t)
  | CAST (t, e) ->
    let e = simpl_casts_exp e in
    let t' = typeof e in
    if equal_typ t t' then e else CAST (t, e)
  | (CONST_INT _ | VARIABLE _) as e -> e

and simpl_casts_stmt : stmt -> stmt = function
  | NOP -> NOP
  | BLOCK (tenv, s) -> BLOCK (tenv, simpl_casts_stmt s)
  | ASSIGN (v, e) -> ASSIGN (v, simpl_casts_exp e)
  | CALL (f, args) ->
    CALL (simpl_casts_exp f, List.map args ~f:simpl_casts_exp)
  | CALLASSIGN (v, f, args) ->
    CALLASSIGN (v, simpl_casts_exp f, List.map args ~f:simpl_casts_exp)
  | STORE (l, r) -> STORE (simpl_casts_exp l, simpl_casts_exp r)
  | SEQUENCE (s1, s2) -> SEQUENCE (simpl_casts_stmt s1, simpl_casts_stmt s2)
  | IF (cond, st, sf) ->
    IF (simpl_casts_exp cond, simpl_casts_stmt st, simpl_casts_stmt sf)
  | GOTO _ as s -> s

and simpl_casts (t : typ) (e : exp) : exp = match simpl_casts_exp e with
  | CAST (t', e) when equal_typ t t' -> e
  | e -> e

(* Propagation of simple expressions *)

module Prop = struct

  module Env = struct

    type t = {
      target : Theory.target;
      prop : exp String.Map.t;
    }

    let insert (e : exp) (v : string) (env : t) : t =
      {env with prop = Map.set env.prop ~key:v ~data:e}

    let lookup (v : string) (env : t) : exp option =
      Map.find env.prop v

  end

  include Monad.State.T1(Env)(Monad.Ident)
  include Monad.State.Make(Env)(Monad.Ident)

end

open Prop.Let

type 'a prop = 'a Prop.t

let rec prop_exp : exp -> exp prop = function
  | UNARY (u, e, t) ->
    let+ e = prop_exp e in
    UNARY (u, e, t)
  | BINARY (b, l, r, t) ->
    let* l = prop_exp l in
    let+ r = prop_exp r in
    BINARY (b, l, r, t)
  | CAST (t, e) ->
    let+ e = prop_exp e in
    CAST (t, e)
  | CONST_INT _ as e -> Prop.return e
  | VARIABLE (v, _) as e ->
    let+ e' = Prop.(gets @@ Env.lookup @@ Theory.Var.name v) in
    Option.value e' ~default:e

and prop_stmt : stmt -> stmt prop = function
  | NOP -> Prop.return NOP
  | BLOCK (tenv, s) ->
    let* {prop; _} = Prop.get () in
    let* s = prop_stmt s in
    let+ () = Prop.update @@ fun env ->
      (* Variables that were introduced in this scope must be removed
         from the environment since we are now leaving this scope. *)
      let prop = Map.filter_keys env.prop ~f:(Map.mem prop) in
      {env with prop} in
    BLOCK (tenv, s)
  | ASSIGN ((v, t), e) -> begin
      let* e = prop_exp e in
      match e with
      | VARIABLE v' when equal_var (v, t) v' -> Prop.return NOP
      | _ when Theory.Var.is_virtual v ->
        let+ () = Prop.(update @@ Env.insert e @@ Theory.Var.name v) in
        NOP
      | _ -> Prop.return @@ ASSIGN ((v, t), e)
    end
  | CALL (f, args) ->
    let* f = prop_exp f in
    let+ args = Prop.List.map args ~f:prop_exp in
    CALL (f, args)
  | CALLASSIGN (v, f, args) ->
    let* f = prop_exp f in
    let+ args = Prop.List.map args ~f:prop_exp in
    CALLASSIGN (v, f, args)
  | STORE (l, r) ->
    let* l = prop_exp l in
    let+ r = prop_exp r in
    STORE (l, r)
  | SEQUENCE (s1, s2) ->
    let* s1 = prop_stmt s1 in
    let+ s2 = prop_stmt s2 in
    SEQUENCE (s1, s2)
  | IF (cond, st, sf) ->
    (* We need to be careful to restore the old environment since each
       branch may have introduced new scopes or clobbered old assignments. *)
    let* {prop; _} = Prop.get () in
    let* cond = prop_exp cond in
    let* st = prop_stmt st in
    let* {prop = pt; _} = Prop.get () in
    let* () = Prop.update @@ fun env -> {env with prop} in
    let* sf = prop_stmt sf in
    let* {prop = pf; _} = Prop.get () in
    (* However, we can merge them if we know that the results will be the
       same (and the variable is within scope). *)
    let prop = Map.merge pt pf ~f:(fun ~key -> function
        | `Left _ | `Right _ -> None
        | `Both (e1, e2) when Map.mem prop key && equal_exp e1 e2 -> Some e1
        | `Both _ -> None) in
    let+ () = Prop.update @@ fun env -> {env with prop} in
    IF (cond, st, sf)
  | GOTO _ as s -> Prop.return s

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
  (* Perform type-checking and elaboration. *)
  let* (tenv, s), _ =
    Transl.(Env.create ~target () |> run (translate_body body)) in
  (* Perform some simplification passes. *)
  let s = simpl_casts_stmt s in
  let s = Monad.State.eval (prop_stmt s) {target; prop = String.Map.empty} in
  let s = cleanup_nop_sequences s in
  let prog = tenv, s in
  Events.send @@ Rule;
  Events.send @@ Info "Translated to the following SmallC program:";
  Events.send @@ Info "";
  Events.send @@ Info (to_string prog);
  Events.send @@ Rule;
  KB.return prog
