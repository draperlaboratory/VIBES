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
  | VOID
  | INT of size * sign
  | PTR of typ
  | FUN of typ * typ list
[@@deriving equal]

let rec string_of_typ : typ -> string = function
  | VOID -> "void"
  | INT (`r8, SIGNED) -> "char"
  | INT (`r8, UNSIGNED) -> "unsigned char"
  | INT (`r16, SIGNED) -> "short"
  | INT (`r16, UNSIGNED) -> "unsigned short"
  | INT (`r32, SIGNED) -> "int"
  | INT (`r32, UNSIGNED) -> "unsigned int"
  | INT (`r64, SIGNED) -> "long long"
  | INT (`r64, UNSIGNED) -> "unsigned long long"
  | INT _ -> assert false
  | PTR t -> sprintf "%s*" @@ string_of_typ t
  | FUN (ret, args) ->
    sprintf "%s (*)(%s)"
      (string_of_typ ret)
      (String.concat ~sep:", " @@ List.map args ~f:string_of_typ)

let size_of_typ (target : Theory.target) : typ -> int = function
  | VOID -> 0
  | INT (size, _) -> Size.in_bits size
  | PTR _ | FUN _ -> Theory.Target.bits target

let sign_of_typ : typ -> sign = function
  | VOID -> UNSIGNED
  | INT (_, s) -> s
  | PTR _ | FUN _ -> UNSIGNED

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

(* Helper for generating a sequence from a list of statements. *)
let sequence : stmt list -> stmt =
  List.fold_right ~init:NOP ~f:(fun s acc -> SEQUENCE (s, acc))

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
  | VOID                 -> Cabs.VOID
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
  | FUN (ret, args) ->
    let names = List.map args ~f:(fun t ->
        let t = cabs_of_typ t in
        t, Cabs.NO_STORAGE, ("", t, Cabs.[GNU_NONE], Cabs.NOTHING)) in
    let ret = cabs_of_typ ret in
    Cabs.PROTO (ret, names, false)

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

let string_of_exp (e : exp) : string =
  Utils.print_c Cprint.print_statement @@ COMPUTATION (cabs_of_exp e)

let string_of_stmt (s : stmt) : string =
  Utils.print_c Cprint.print_statement @@ cabs_of_stmt s

let to_string ((tenv, s) : t) : string =
  let vars =
    Map.to_alist tenv |> List.map ~f:(fun (v, t) ->
        sprintf "%s %s;" (string_of_typ t) v) |>
    String.concat ~sep:"\n" in
  let stmt = Utils.print_c Cprint.print_statement @@ cabs_of_stmt s in
  sprintf "%s\n%s" vars stmt

(* Returns true if an expression is an l-value (i.e. can be mutated). *)
let rec is_lvalue : Cabs.expression -> bool = function
  | Cabs.(UNARY (MEMOF, e)) -> is_lvalue e
  | Cabs.(BINARY (ASSIGN, e, _)) -> is_lvalue e
  | Cabs.(BINARY (ADD_ASSIGN, e, _))
  | Cabs.(BINARY (SUB_ASSIGN, e, _))
  | Cabs.(BINARY (MUL_ASSIGN, e, _))
  | Cabs.(BINARY (DIV_ASSIGN, e, _))
  | Cabs.(BINARY (MOD_ASSIGN, e, _))
  | Cabs.(BINARY (BAND_ASSIGN, e, _))
  | Cabs.(BINARY (BOR_ASSIGN, e, _))
  | Cabs.(BINARY (SHL_ASSIGN, e, _))
  | Cabs.(BINARY (SHR_ASSIGN, e, _))
  | Cabs.(INDEX (e, _)) -> is_lvalue e
  | Cabs.(VARIABLE _) -> true
  | Cabs.(QUESTION (_, l, r)) -> is_lvalue l && is_lvalue r
  | _ -> false

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

(* Translate a base type. *)
let rec translate_type
    ?(msg : string = "")
    (t : Cabs.base_type) : typ transl = match t with
  | Cabs.VOID -> Transl.return VOID
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
    let+ t = translate_type t ~msg in
    PTR t
  | Cabs.PROTO (_, _, true) ->
    let s = Utils.print_c (Cprint.print_type ident) t in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_type: %sVariadic functions are \
               unsupported:\n\n%s\n" msg s)
  | Cabs.PROTO (ret, names, false) ->
    let* ret = translate_type ret ~msg in
    let+ args =
      Transl.List.map names ~f:(fun (_, _, (_, t, _, _)) ->
          translate_type t ~msg) in
    FUN (ret, args)
  | _ ->
    let s = Utils.print_c (Cprint.print_type ident) t in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_type: %sunsupported type:\n\n%s" msg s)

(* See whether the elements of the pointer type can unify. *)
let rec typ_unify_ptr (t1 : typ) (t2 : typ) : typ option =
  match t1, t2 with
  | PTR t1, PTR t2 -> typ_unify_ptr t1 t2 |> Option.map ~f:(fun t -> PTR t)
  | VOID, _ -> Some t2
  | _, VOID -> Some t1
  | _ -> if equal_typ t1 t2 then Some t1 else None

(* Perform type conversions for pure expressions. *)
let typ_unify (t1 : typ) (t2 : typ) : typ option =
  match t1, t2 with
  | VOID, VOID -> Some VOID
  | VOID, _ | _, VOID -> None
  | INT _, PTR _ | PTR _, INT _ -> None
  | _, FUN _ | FUN _, _ -> None
  | PTR t1', PTR t2' ->
    typ_unify_ptr t1' t2' |> Option.map ~f:(fun t -> PTR t)
  | INT (size1, sign1), INT (size2, sign2) ->
    match Size.compare size1 size2 with
    | n when n < 0 -> Some t2
    | n when n > 0 -> Some t1
    | _ -> match sign1, sign2 with
      | SIGNED, UNSIGNED | UNSIGNED, SIGNED ->
        Some (INT (size1, UNSIGNED))
      | _ -> Some t1

(* Cast pointers to integers. *)
let rec typ_unify_ptr_to_int (bits : int) (t1 : typ) (t2 : typ) : typ option =
  match t1, t2 with
  | PTR _, INT (_, sign) -> Some (INT (Size.of_int_exn bits, UNSIGNED))
  | INT (_, sign), PTR _ -> Some (INT (Size.of_int_exn bits, UNSIGNED))
  | PTR _, PTR _ -> Some (PTR VOID)
  | (INT _ | PTR _), FUN (ret, _) -> typ_unify_ptr_to_int bits t1 ret
  | FUN (ret, _), (INT _ | PTR _) -> typ_unify_ptr_to_int bits ret t2
  | INT _, INT _ -> typ_unify t1 t2
  | _ -> None

(* Same as `typ_unify_ptr` but favor the lhs. *)
let rec typ_unify_ptr_assign (t1 : typ) (t2 : typ) : typ option =
  match t1, t2 with
  | PTR t1, PTR t2 -> typ_unify_ptr_assign t1 t2
  | VOID, _ | _, VOID -> Some t1
  | _ -> if equal_typ t1 t2 then Some t1 else None

(* Perform type conversions for an assignment. Returns the unified type
   and the expression with an explicit cast. *)
let typ_unify_assign (tl : typ) (tr : typ) (r : exp) : (typ * exp) option =
  match tl, tr with
  | VOID, _ | _, VOID -> None
  | INT _, PTR _ | PTR _, INT _ -> None
  | FUN (rl, al), FUN (rr, ar) -> begin
      match typ_unify rl rr with
      | None -> None
      | Some ret ->
        try
          let a = List.zip_exn al ar |> List.map ~f:(fun (l, r) ->
              let t = typ_unify l r in
              Option.value_exn t) in
          Some (FUN (ret, a), r)
        with _ -> None
    end
  | FUN _, _ | _, FUN _ -> None
  | PTR t1', PTR t2' ->
    typ_unify_ptr_assign t1' t2' |>
    Option.map ~f:(fun t -> tl, with_type r tl)
  | INT (sizel, signl), INT (sizer, signr) ->
    if equal_size sizel sizer && equal_sign signl signr
    then Some (tl, r) else Some (tl, with_type r tl)

(* The elaboration will leave a bunch of nops in the AST which makes
   pretty-printing quite ugly. This pass removes them. *)
let rec cleanup_nop_sequences (s : stmt) : stmt = match s with
  | NOP -> NOP
  | BLOCK (tenv, s) -> begin
      match cleanup_nop_sequences s with
      | NOP -> NOP
      | s -> BLOCK (tenv, s)
    end
  | ASSIGN ((v1, _), VARIABLE (v2, _)) when Theory.Var.Top.(v1 = v2) -> NOP
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
  | IF (cond, st, sf) -> begin
      let st = cleanup_nop_sequences st in
      let sf = cleanup_nop_sequences sf in
      match st, sf with
      | NOP, NOP -> NOP
      | _ -> IF (cond, st, sf)
    end
  | GOTO _ -> s

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
      let+ spre, e', spost = translate_expression e in
      match e' with
      | None -> sequence [acc; spre; spost]
      | Some e' -> sequence [acc; spre; ASSIGN (v, e'); spost])

and typ_unify_error : 'a. Cabs.expression -> typ -> typ -> 'a transl =
  fun e t1 t2 ->
  let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
  let s1 = string_of_typ t1 in
  let s2 = string_of_typ t2 in
  Transl.fail @@ Core_c_error (
    sprintf "Failed to unify types %s and %s in expression:\n\n%s\n" s1 s2 s)

and typ_error (e : Cabs.expression) (t : typ) (msg : string) : 'a transl =
  let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
  let t = string_of_typ t in
  Transl.fail @@ Core_c_error (
    sprintf "Expression:\n\n%s\n\nunified to type %s. %s\n" s t msg)

(* Translate an expression which may be `None`. Also returns any side effects
   produced by the expression. We separate effects that happen before the
   expression is evaluated, and effects that happen after. This is to handle
   compound expressions on lvalues, as well as pre and post increment
   operators.

   `assign` denotes whether we want the result of evaluating this expression
   to be assigned to a particular variable. Otherwise, a fresh temporary is
   created to hold the result

   `computation` denotes whether this expression was derived from a FrontC
   COMPUTATION statement. This means that the expression is being evaluated
   for its side effects only, and the result may then be discarded.
*)
and translate_expression
    ?(assign : var option = None)
    ?(computation = false)
    (e : Cabs.expression) : (stmt * exp option * stmt) transl =
  match e with
  | Cabs.NOTHING -> Transl.return (NOP, None, NOP)
  | Cabs.UNARY (u, e) ->
    let+ spre, e, spost = translate_unary_operator u e in
    if computation then spre, None, spost else spre, e, spost
  | Cabs.BINARY (b, lhs, rhs) ->
    let+ spre, e, spost = translate_binary_operator b lhs rhs in
    if computation then spre, None, spost else spre, e, spost
  | Cabs.QUESTION (cond, then_, else_) ->
    let+ spre, e, spost = translate_question cond then_ else_ ~assign in
    spre, e, spost
  | Cabs.CAST (t, e) ->
    let* t = translate_type t in
    let+ spre, e', spost =
      translate_expression_strict "translate_expression (CAST)" e ~assign in
    if computation then spre, None, spost
    else spre, Some (CAST (t, e')), spost
  | Cabs.CALL (f, args) ->
    let+ s, e = translate_call f args ~assign ~computation in
    s, e, NOP
  | Cabs.CONSTANT _ when computation -> Transl.return (NOP, None, NOP)
  | Cabs.(CONSTANT (CONST_INT s)) ->
    let i = Int64.of_string s in
    let sign = if Int64.is_negative i then SIGNED else UNSIGNED in
    let width =
      (* Fits inside a byte. *)
      if Int64.(i >= -128L && i <= 127L) then 8
      (* Fits inside a halfword. *)
      else if Int64.(i >= -32_768L && i <= 32_767L) then 16
      (* Fits inside a word. *)
      else if Int64.(i >= -2_147_483_648L && i <= 2_147_483_647L) then 32
      (* Fits inside a long. *)
      else 64 in
    let i = Word.of_int64 ~width i in
    (* `word` is unsigned by default. *)
    let i = if equal_sign sign SIGNED then Word.signed i else i in
    Transl.return (NOP, Some (CONST_INT (i, sign)), NOP)
  | Cabs.(CONSTANT (CONST_CHAR s)) ->
    let i = Word.of_int ~width:8 Char.(to_int @@ of_string s) in
    let i = Word.signed i in
    Transl.return (NOP, Some (CONST_INT (i, SIGNED)), NOP)
  | Cabs.VARIABLE _ when computation -> Transl.return (NOP, None, NOP)
  | Cabs.VARIABLE v -> begin
      let* t = Transl.(gets @@ Env.typeof v) in
      match t with
      | Some t ->
        let+ {target; _} = Transl.get () in
        let s = Theory.Bitv.define @@ size_of_typ target t in
        let v = Theory.Var.define s v |> Theory.Var.forget in
        NOP, Some (VARIABLE (v, t)), NOP
      | None ->
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_expression: undeclared variable %s\n" v)
    end
  | Cabs.EXPR_SIZEOF _ when computation -> Transl.return (NOP, None, NOP)
  | Cabs.EXPR_SIZEOF e ->
    let* _, e, _ =
      translate_expression_strict "translate_expression (EXPR_SIZEOF)" e in
    let+ {target; _} = Transl.get () in
    let width = Theory.Target.bits target in
    let size = size_of_typ target @@ typeof e in
    NOP, Some (CONST_INT (Word.of_int ~width (size lsr 3), UNSIGNED)), NOP
  | Cabs.TYPE_SIZEOF t ->
    let s = Utils.print_c Cprint.print_base_type t in
    let* t = translate_type t ~msg:(sprintf "In expression %s: " s) in
    let+ {target; _} = Transl.get () in
    let width = Theory.Target.bits target in
    let size = size_of_typ target t in
    NOP, Some (CONST_INT (Word.of_int ~width (size lsr 3), UNSIGNED)), NOP
  | Cabs.INDEX (ptr, idx) ->
    let+ spre, e, t, spost = translate_index ptr idx in
    spre, Some (UNARY (MEMOF, CAST (PTR t, e), t)), spost
  | _ ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_expression: unsupported:\n\n%s\n" s)

(* Translate an expression and expect that the value is not `None`. *)
and translate_expression_strict
    ?(assign : var option = None)
    (stage : string)
    (e : Cabs.expression) : (stmt * exp * stmt) transl =
  let* spre, e', spost = translate_expression e ~assign in
  match e' with
  | Some e' -> Transl.return (spre, e', spost)
  | None ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.%s: invalid expression:\n\n%s\n" stage s)

(* Translate an expression that must be an l-value. *)
and translate_expression_lvalue
    ?(assign : var option = None)
    (stage : string)
    (e : Cabs.expression) : (stmt * exp * stmt) transl =
  if is_lvalue e
  then translate_expression_strict stage e ~assign
  else
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.%s: expression:\n\n%s\n\nis not an l-value" stage s)

(* Translate unary operators. *)
and translate_unary_operator
    (u : Cabs.unary_operator)
    (e : Cabs.expression) : (stmt * exp option * stmt) transl =
  let lval = translate_expression_lvalue "translate_unary_operator" in
  let exp = translate_expression_strict "translate_unary_operator" in
  match u with
  | Cabs.MINUS ->
    let+ spre, e, spost = exp e in
    spre, Some (UNARY (MINUS, e, typeof e)), spost
  | Cabs.PLUS ->
    let+ spre, e, spost = exp e in
    spre, Some e, spost
  | Cabs.NOT ->
    let* spre, e, spost = exp e in
    let+ {target; _} = Transl.get () in
    let width = size_of_typ target @@ typeof e in
    let i = Word.zero width in
    spre, Some (BINARY (NE, e, CONST_INT (i, UNSIGNED),typeof e)), spost
  | Cabs.BNOT ->
    let+ spre, e, spost = exp e in
    spre, Some (UNARY (LNOT, e, typeof e)), spost
  | Cabs.MEMOF -> begin
      let* spre, e', spost = exp e in
      match typeof e' with
      | PTR VOID ->
        let s =
          Utils.print_c Cprint.print_statement
            Cabs.(COMPUTATION (UNARY (u, e))) in
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_unary_operator: in expression\
                   \n\n%s\n\ncannot dereference a value of type void*" s)
      | PTR t -> Transl.return (spre, Some (UNARY (MEMOF, e', t)), spost)
      | _ ->
        let s =
          Utils.print_c Cprint.print_statement
            Cabs.(COMPUTATION (UNARY (u, e))) in
        Transl.fail @@ Core_c_error (
          sprintf "Smallc.translate_unary_operator: expected pointer type \
                   for operand of expression:\n\n%s\n" s)
    end
  | Cabs.ADDROF ->
    let+ spre, e', spost = lval e in
    spre, Some (UNARY (ADDROF, e', PTR (typeof e'))), spost
  | Cabs.PREINCR ->
    let* spre, e', spost, expanded = translate_increment_operand u e in
    if expanded then Transl.return (spre, Some e', spost)
    else
      let* spre, e', spost = lval e in
      let n = CONST_INT (Word.one 8, UNSIGNED) in
      let* rhs = make_arith ADD e' n in
      let+ eff = make_assign e' rhs in
      sequence [spre; eff], Some e', spost
  | Cabs.POSINCR ->
    let* spre, e', spost, expanded = translate_increment_operand u e in
    if expanded then Transl.return (spre, Some e', spost)
    else
      let* spre, e', spost = lval e in
      let n = CONST_INT (Word.one 8, UNSIGNED) in
      let* rhs = make_arith ADD e' n in
      let+ eff = make_assign e' rhs in
      spre, Some e', sequence [spost; eff]
  | Cabs.PREDECR ->
    let* spre, e', spost, expanded = translate_increment_operand u e in
    if expanded then Transl.return (spre, Some e', spost)
    else
      let n = CONST_INT (Word.one 8, UNSIGNED) in
      let* rhs = make_arith SUB e' n in
      let+ eff = make_assign e' rhs in
      sequence [spre; eff], Some e', spost
  | Cabs.POSDECR ->
    let* spre, e', spost, expanded = translate_increment_operand u e in
    if expanded then Transl.return (spre, Some e', spost)
    else
      let n = CONST_INT (Word.one 8, UNSIGNED) in
      let* rhs = make_arith SUB e' n in
      let+ eff = make_assign e' rhs in
      spre, Some e', sequence [spost; eff]

(* Expand the increment into the operand if necessary. *)
and translate_increment_operand
    (u : Cabs.unary_operator)
    (e : Cabs.expression) : (stmt * exp * stmt * bool) transl =
  let lval = translate_expression_lvalue "translate_increment_operand" in
  if is_lvalue e then match e with
    | Cabs.QUESTION (c, l, r) ->
      let+ pre, e, post =
        translate_question c
          Cabs.(UNARY (u, l)) Cabs.(UNARY (u, r)) in
      let e = Option.value_exn e in
      pre, e, post, true
    | _ ->
      let+ pre, e, post = lval e in
      pre, e, post, false
  else
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_increment_operand: \
               expression:\n\n%s\n\nis not an l-value" s)

(* Increment value based on the type. *)
and increment (e : Cabs.expression) (t : typ) : exp transl =
  let* bits = Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
  match t with
  | PTR (INT (size, _)) ->
    (* Pointer to some element type, use the element size. *)
    let i = Word.of_int ~width:bits @@ Size.in_bytes size in
    Transl.return @@ CONST_INT (i, UNSIGNED)
  | PTR (PTR _ | FUN _ | VOID) ->
    (* Pointer to a pointer: use the word size. *)
    let i = Word.of_int ~width:bits (bits lsr 3) in
    Transl.return @@ CONST_INT (i, UNSIGNED)
  | FUN _ | VOID ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    let t = string_of_typ t in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.increment: expression:\n\n%s\n\n\
               has type %s. Cannot be an l-value." s t)
  | INT (size, sign) ->
    let i = Word.one bits in
    let i = if equal_sign sign SIGNED then Word.signed i else i in
    Transl.return @@ CONST_INT (i, sign)

(* If the expression had side-effects, then store the result in a
   temporary variable.

   `no_post` indicates that a temporary shouldn't be generated if
   there are no post-effects.
*)
and new_tmp_or_simple
    ?(no_post : bool = false)
    (pre : stmt)
    (e : exp)
    (post : stmt) : (exp, stmt * exp * stmt) Either.t transl =
  let pre = cleanup_nop_sequences pre in
  let post = cleanup_nop_sequences post in
  match pre, post with
  | NOP, NOP -> Transl.return @@ First e
  | _, NOP -> Transl.return @@ Second (pre, e, post)
  | _ ->
    let+ tmp = new_tmp @@ typeof e in
    let pre = sequence [pre; ASSIGN (tmp, e)] in
    Second (pre, VARIABLE tmp, post)

(* Helper for binary operators where the sequencing of side-effects
   for the operands is important. *)
and binary_tmp_or_simple
    ?(no_post : bool = false)
    (op : binop)
    (pre1 : stmt)
    (e1 : exp)
    (post1 : stmt)
    (pre2 : stmt)
    (e2 : exp)
    (post2 : stmt)
    (t : typ)
    ~(f : binop -> exp -> exp -> typ -> exp transl)
  : (stmt * exp option * stmt) transl =
  (* Evaluate left to right. *)
  let* te1 = new_tmp_or_simple pre1 e1 post1 in
  let* te2 = new_tmp_or_simple pre2 e2 post2 ~no_post in
  match te1, te2 with
  | First e1, First e2 ->
    let+ e = f op e1 e2 t in
    NOP, Some e, NOP
  | First e1, Second (pre, e2, post) ->
    let* tmp = new_tmp t in
    let eff = sequence [
        ASSIGN (tmp, e1);
        pre;
        post;
      ] in
    let+ e = f op (VARIABLE tmp) e2 t in
    eff, Some e, NOP
  | Second (pre, e1, post), First e2 ->
    let* tmp = new_tmp t in
    let eff = sequence [
        pre;
        post;
        ASSIGN (tmp, e2);
      ] in
    let+ e = f op e1 (VARIABLE tmp) t in
    eff, Some e, NOP
  | Second (pre1, e1, post1), Second (pre2, e2, post2) ->
    let eff = sequence [
        pre1;
        post1;
        pre2;
        post2;
      ] in
    let+ e = f op e1 e2 t in
    eff, Some e, NOP

(* Translate binary operators. *)
and translate_binary_operator
    (b : Cabs.binary_operator)
    (lhs : Cabs.expression)
    (rhs : Cabs.expression) : (stmt * exp option * stmt) transl =
  let exp = translate_expression_strict "translate_binary_operator" in
  let default ?(no_ptr = false) op =
    let* spre1, e1, spost1 = exp lhs in
    let t1 = typeof e1 in
    let* spre2, e2, spost2 = exp rhs in
    let t2 = typeof e2 in
    match typ_unify t1 t2 with
    | None -> typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
    | Some (PTR _) when no_ptr ->
      typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
    | Some t ->
      let e1 = with_type e1 t in
      let e2 = with_type e2 t in
      let f op e1 e2 t = Transl.return @@ BINARY (op, e1, e2, t) in
      binary_tmp_or_simple op spre1 e1 spost1 spre2 e2 spost2 t ~f
        ~no_post:true in
  match b with
  | Cabs.ADD -> translate_arith ADD b lhs rhs
  | Cabs.SUB -> translate_arith SUB b lhs rhs
  | Cabs.MUL -> translate_arith MUL b lhs rhs ~no_ptr:true
  | Cabs.DIV -> translate_arith DIV b lhs rhs ~no_ptr:true
  | Cabs.MOD -> translate_arith MOD b lhs rhs ~no_ptr:true
  | Cabs.AND -> begin
      (* Short-circuiting boolean AND *)
      let* bits =
        Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
      let* spre1, e1, spost1 = exp lhs in
      let t1 = typeof e1 in
      let* spre2, e2, spost2 = exp rhs in
      let t2 = typeof e2 in
      match typ_unify_ptr_to_int bits t1 t2 with
      | None -> typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
      | Some t ->
        let e1 = with_type e1 t in
        let e2 = with_type e2 t in
        let+ tmp = new_tmp t in
        let eff = sequence [
            spre1;
            ASSIGN (tmp, e1);
            spost1;
            IF (
              VARIABLE tmp,
              sequence [
                spre2;
                ASSIGN (tmp, e2);
                spost2
              ],
              NOP);
          ] in
        eff, Some (VARIABLE tmp), NOP
    end
  | Cabs.OR -> begin
      (* Short-circuiting boolean OR *)
      let* bits =
        Transl.gets @@ fun {target; _} -> Theory.Target.bits target in
      let* spre1, e1, spost1 = exp lhs in
      let t1 = typeof e1 in
      let* spre2, e2, spost2 = exp rhs in
      let t2 = typeof e2 in
      match typ_unify_ptr_to_int bits t1 t2 with
      | None -> typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
      | Some t ->
        let e1 = with_type e1 t in
        let e2 = with_type e2 t in
        let+ tmp = new_tmp t in
        let eff = sequence [
            spre1;
            ASSIGN (tmp, e1);
            spost1;
            IF (
              VARIABLE tmp,
              NOP,
              sequence [
                spre2;
                ASSIGN (tmp, e2);
                spost2;
              ]);
          ] in
        eff, Some (VARIABLE tmp), NOP
    end
  | Cabs.BAND -> default LAND ~no_ptr:true
  | Cabs.BOR -> default LOR ~no_ptr:true
  | Cabs.XOR -> default XOR ~no_ptr:true
  | Cabs.SHL -> default SHL ~no_ptr:true
  | Cabs.SHR -> default SHR ~no_ptr:true
  | Cabs.EQ -> default EQ
  | Cabs.NE -> default NE
  | Cabs.LT -> default LT
  | Cabs.GT -> default GT
  | Cabs.LE -> default LE
  | Cabs.GE -> default GE
  | Cabs.ASSIGN -> translate_assign lhs rhs
  | Cabs.ADD_ASSIGN -> translate_compound ADD b lhs rhs
  | Cabs.SUB_ASSIGN -> translate_compound SUB b lhs rhs
  | Cabs.MUL_ASSIGN -> translate_compound MUL b lhs rhs ~no_ptr:true
  | Cabs.DIV_ASSIGN -> translate_compound DIV b lhs rhs ~no_ptr:true
  | Cabs.MOD_ASSIGN -> translate_compound MOD b lhs rhs ~no_ptr:true
  | Cabs.BAND_ASSIGN -> translate_compound LAND b lhs rhs ~no_ptr:true
  | Cabs.BOR_ASSIGN -> translate_compound LOR b lhs rhs ~no_ptr:true
  | Cabs.XOR_ASSIGN -> translate_compound XOR b lhs rhs ~no_ptr:true
  | Cabs.SHL_ASSIGN -> translate_compound SHL b lhs rhs ~no_ptr:true
  | Cabs.SHR_ASSIGN -> translate_compound SHR b lhs rhs ~no_ptr:true

and translate_compound
    ?(no_ptr : bool = false)
    ?(e : Cabs.expression = NOTHING)
    (b : binop)
    (b' : Cabs.binary_operator)
    (lhs : Cabs.expression)
    (rhs : Cabs.expression) : (stmt * exp option * stmt) transl =
  let lval = translate_expression_lvalue "translate_compound" in
  let exp = translate_expression_strict "translate_compound" in
  match lhs with
  | Cabs.QUESTION (c, l, r) when is_lvalue lhs ->
    translate_question c
      Cabs.(BINARY (b', l, rhs)) Cabs.(BINARY (b', r, rhs))
  | _ ->
    let* spre1, e1, spost1 = lval lhs in
    let* spre2, e2, spost2 = exp rhs in
    let e = Cabs.(BINARY (b', lhs, rhs)) in
    let* e' = make_arith b e1 e2 ~e ~no_ptr in
    translate_assign_aux spre1 e1 spost1 spre2 e' spost2 ~e ~lhs

and translate_arith
    ?(no_ptr : bool = false)
    (b : binop)
    (b' : Cabs.binary_operator)
    (lhs : Cabs.expression)
    (rhs : Cabs.expression) : (stmt * exp option * stmt) transl =
  let exp = translate_expression_strict "translate_arithmetic" in
  let e = Cabs.(BINARY (b', lhs, rhs)) in
  let* spre1, e1, spost1 = exp lhs in
  let* spre2, e2, spost2 = exp rhs in
  let f op e1 e2 _ = make_arith op e1 e2 ~e ~no_ptr in
  binary_tmp_or_simple b spre1 e1 spost1 spre2 e2 spost2 VOID ~f ~no_post:true

(* Generate an arithmetic expression depending on whether pointer
   arithmetic is allowed. *)
and make_arith
    ?(no_ptr : bool = false)
    ?(e : Cabs.expression = Cabs.NOTHING)
    (b : binop)
    (e1 : exp)
    (e2 : exp) : exp transl =
  let t1 = typeof e1 in
  let t2 = typeof e2 in
  match t1, t2 with
  | PTR VOID, _ | _, PTR VOID -> typ_unify_error e t1 t2
  | (PTR _, _ | _, PTR _) when no_ptr -> typ_unify_error e t1 t2
  | PTR _, PTR _ -> typ_unify_error e t1 t2
  | _ ->
    let+ t, e1, e2 = match t1, t2 with
      | PTR _, INT _ ->
        let+ inc = increment NOTHING t1 in
        t1, e1, BINARY (MUL, e2, inc, t2)
      | INT _, PTR _ ->
        let+ inc = increment NOTHING t2 in
        t2, BINARY (MUL, e1, inc, t1), e2
      | INT _, INT _ ->
        let t = typ_unify t1 t2 in
        let t = Option.value_exn t in
        Transl.return (t, e1, e2)
      | _ -> typ_unify_error e t1 t2 in
    BINARY (b, e1, e2, t)

and translate_assign
    ?(lhs_pre : exp option = None)
    (lhs : Cabs.expression)
    (rhs : Cabs.expression) : (stmt * exp option * stmt) transl =
  let exp ?(assign = None) =
    translate_expression_strict "translate_assign" ~assign in
  let lval = translate_expression_lvalue "translate_assign" in
  let* spre1, e1, spost1 = match lhs_pre with
    | Some e -> Transl.return (NOP, e, NOP)
    | None -> lval lhs in
  let* spre2, e2, spost2 = match e1 with
    | VARIABLE v -> exp rhs ~assign:(Some v)
    | _ -> exp rhs in
  translate_assign_aux
    spre1 e1 spost1 spre2 e2 spost2 ~lhs
    ~e:Cabs.(BINARY (ASSIGN, lhs, rhs))

(* The operands have been compiled and their side effects are explicit,
   so continue compiling the assignment. *)
and translate_assign_aux
    ?(lhs : Cabs.expression = NOTHING)
    ?(e : Cabs.expression = NOTHING)
    (spre1 : stmt)
    (e1 : exp)
    (spost1 : stmt)
    (spre2 : stmt)
    (e2 : exp)
    (spost2 : stmt) : (stmt * exp option * stmt) transl =
  let* is_store = match e1 with
    | VARIABLE (_, t) -> Transl.return false
    | UNARY (MEMOF, _, t) -> Transl.return true
    | _ ->
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION lhs) in
      Transl.fail @@ Core_c_error (
        sprintf "Csmall.translate_assign: expected an l-value \
                 for LHS of assignment, got:\n\n%s\n" s) in
  (* We follow order of evaluation as right-to-left. *)
  let* te2 = new_tmp_or_simple spre2 e2 spost2 ~no_post:true in
  match te2 with
  | First e2 ->
    let+ s = make_assign e1 e2 ~is_store ~e in
    sequence [spre1; s], Some e1, spost1
  | Second (spre2, e2, spost2) ->
    let+ s = make_assign e1 e2 ~is_store ~e in
    sequence [spre2; spost2; spre1; s], Some e1, spost1

(* Do type checking and either generate an assignment or a store. *)
and make_assign
    ?(is_store : bool = false)
    ?(e : Cabs.expression = NOTHING)
    (e1 : exp)
    (e2 : exp) : stmt transl =
  let t1 = typeof e1 in
  let t2 = typeof e2 in
  match typ_unify_assign t1 t2 e2 with
  | None ->
    let t1 = if is_store then PTR t1 else t1 in
    typ_unify_error e t1 t2
  | Some (_, e2) -> match e1 with
    | VARIABLE var -> Transl.return @@ ASSIGN (var, e2)
    | UNARY (MEMOF, addr, _) -> Transl.return @@ STORE (addr, e2)
    | _ -> Transl.fail @@
      Core_c_error "Csmall.make_assign: unexpected shape"

and translate_question
    ?(lval = false)
    ?(assign : var option = None)
    (cond : Cabs.expression)
    (then_ : Cabs.expression)
    (else_ : Cabs.expression) : (stmt * exp option * stmt) transl =
  let e = Cabs.(QUESTION (cond, then_, else_)) in
  let exp =
    (if lval
     then translate_expression_lvalue
     else translate_expression_strict)
      ~assign "translate_question" in
  let* scondpre, cond, scondpost = exp cond in
  let* sthenpre, ethen, sthenpost = exp then_ in
  let* selsepre, eelse, selsepost = exp else_ in
  let t1 = typeof ethen in
  let t2 = typeof eelse in
  match typ_unify t1 t2 with
  | None -> typ_unify_error e t1 t2
  | Some VOID ->
    let+ dummy = new_tmp VOID in
    let pre = sequence [
        scondpre;
        IF (
          cond,
          sequence [
            scondpost;
            sthenpre;
            sthenpost;
          ],
          sequence [
            scondpost;
            selsepre;
            selsepost;
          ]);
      ] in
    pre, Some (VARIABLE dummy), NOP
  | Some t ->
    let ethen = with_type ethen t in
    let eelse = with_type eelse t in
    let+ v = match assign with
      | Some v -> Transl.return v
      | None -> new_tmp t in
    let pre = sequence [
        scondpre;
        IF (
          cond,
          sequence [
            scondpost;
            sthenpre;
            ASSIGN (v, ethen);
            sthenpost;
          ],
          sequence [
            scondpost;
            selsepre;
            ASSIGN (v, eelse);
            selsepost;
          ]);
      ] in
    pre, Some (VARIABLE v), NOP

and translate_call_args
    ?(e : Cabs.expression = NOTHING)
    (args : Cabs.expression list)
    (targs : typ list) : (stmt * exp * stmt) list transl =
  let exp = translate_expression_strict "translate_call_args" in
  match List.zip args targs with
  | Ok l ->
    (* Evaluated left to right. *)
    Transl.List.fold_right l ~init:[]
      ~f:(fun (arg, t) acc ->
          let* spre, a, spost = exp arg in
          let ta = typeof a in
          match typ_unify t ta with
          | None ->
            let s =
              Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
            let a =
              Utils.print_c Cprint.print_statement Cabs.(COMPUTATION arg) in
            let t = string_of_typ t in
            let ta = string_of_typ ta in
            Transl.fail @@ Core_c_error (
              sprintf "Smallc.translate_call_args:\n\n%s\
                       \n\nargument %s has type %s but type %s was \
                       expected" s a ta t)
          | Some t ->
            let a = with_type a t in
            Transl.return ((spre, a, spost) :: acc))
  | Unequal_lengths ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    let l1 = List.length targs in
    let l2 = List.length args in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_call_args:\n\n%s\n\n\
               expected %d arguments, got %d" s l1 l2)

and translate_call
    ?(assign : var option = None)
    ?(computation : bool = false)
    (f : Cabs.expression)
    (args : Cabs.expression list) : (stmt * exp option) transl =
  let e = Cabs.CALL (f, args) in
  let exp = translate_expression_strict "translate_call" in
  let* sfpre, f', sfpost = exp f in
  match typeof f' with
  | PTR (FUN (tret, targs)) | FUN (tret, targs) ->
    let* args = translate_call_args args targs ~e in
    let* eff, args' =
      (* Sequence the effects of each arg expression. *)
      let rec aux (pre, args, post) = function
        | [] -> Transl.return (sequence [pre; post], List.rev args)
        | (spre, e, spost) :: rest ->
          let no_post = List.is_empty rest in
          let* te = new_tmp_or_simple spre e spost ~no_post in
          let eff, e = match te with
            | First e -> sequence [pre; post], e
            | Second (spre, e, spost) -> sequence [pre; post; spre], e in
          aux (eff, e :: args, spost) rest in
      aux (sfpre, [], sfpost) args in
    let is_void = match typ_unify tret VOID with
      | Some VOID -> true
      | Some _ -> false
      | None -> false in
    if computation
    then Transl.return (sequence [eff; CALL (f', args')], None)
    else if is_void then
      let+ dummy = new_tmp VOID in
      sequence [eff; CALL (f', args')], Some (VARIABLE dummy)
    else
      (* Do we already know who we're assigning to? *)
      let+ v = match assign with
        | None -> new_tmp tret
        | Some (v, t) ->
          (* Type checking. Use a dummy RHS since calls are not
             expressions. *)
          let dummy = CONST_INT (Word.of_int ~width:8 42, UNSIGNED) in
          match typ_unify_assign t tret dummy with
          | Some _ -> Transl.return (v, t)
          | None ->
            let s =
              Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
            let t = string_of_typ t in
            let tret = string_of_typ tret in
            Transl.fail @@ Core_c_error (
              sprintf "Smallc.translate_call:\n\n%s\n\n\
                       has return type %s, cannot unify with var %s of \
                       type %s"
                s tret (Theory.Var.name v) t) in
      sequence [eff; CALLASSIGN (v, f', args')], Some (VARIABLE v)
  | t ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    let t = string_of_typ t in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_call:\n\n%s\n\n\
               has type %s, expected function type" s t)

(* This function returns the side effects, the pointer to the element in the
   array (as an integer), and the element type. *)
and translate_index
    (ptr : Cabs.expression)
    (idx : Cabs.expression) : (stmt * exp * typ * stmt) transl =
  let exp = translate_expression_strict "translate_index" in
  let* sptrpre, eptr, sptrpost = exp ptr in
  let* sidxpre, eidx, sidxpost = exp idx in
  let tptr = typeof eptr in
  let tidx = typeof eidx in
  match tptr, tidx with
  | PTR t, INT _ ->
    (* Translate to the pointer arithmetic of an array lookup.

       NOTE: we're not supporting array types currently. A multidimensional
       lookup, if the type is an array, must assume that the memory layout
       is flat. The semantics of a multidimensional lookup of a pointer type
       is captured here.

       Example:

       int f(int **x) {
         return x[1][2];
       }

       This function is compiled to the following ARM code by GCC:

       ldr     r3, [r0, #4]
       ldr     r0, [r3, #8]
       bx      lr

       Whereas the following function:

       int f(int x[8][8]) {
         return x[1][2];
       }

       is compiled to:

       ldr     r0, [r0, #40]
       bx      lr

       The math behind it is:

       i = x + y * w 

       So for this example:

       x = 2 * 4
       y = 1 * 4
       w = 8
       i = (2 * 4) + (1 * 4 * 8) = 8 + 32 = 40

       We multiply by 4 since that is the size of an `int`.
    *)
    let+ {target; _} = Transl.get () in
    let bits = Theory.Target.bits target in
    let scale = Word.of_int ~width:bits (size_of_typ target t lsr 3) in
    let tidx = INT (Size.of_int_exn bits, UNSIGNED) in
    let eidx = with_type eidx tidx in
    let e =
      BINARY (
        ADD,
        CAST (tidx, eptr),
        BINARY (
          MUL,
          CONST_INT (scale, UNSIGNED),
          eidx,
          tidx),
        tidx) in
    sequence [sptrpre; sidxpre], e, t, sequence [sptrpost; sidxpost]
  | PTR _, _ ->
    let e = Cabs.(INDEX (ptr, idx)) in
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    let t = string_of_typ tidx in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_index: in expression:\n\n%s\n\nIndex operand \
               has type %s. Expected integer.\n" s t)
  | _, _ ->
    let e = Cabs.(INDEX (ptr, idx)) in
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    let t = string_of_typ tptr in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_index: in expression:\n\n%s\n\nArray operand \
               has type %s. Expected pointer.\n" s t)

(* Translate a statement. *)
and translate_statement (s : Cabs.statement) : stmt transl = match s with
  | Cabs.NOP -> Transl.return NOP
  | Cabs.COMPUTATION e -> begin
      let* spre, e, spost = translate_expression e ~computation:true in
      match e with
      | None -> Transl.return @@ sequence [spre; spost]
      | Some e ->
        let+ v = new_tmp @@ typeof e in
        sequence [spre; ASSIGN (v, e); spost]
    end
  | Cabs.BLOCK body ->
    let+ body = translate_body body in
    BLOCK body
  | Cabs.SEQUENCE (s1, s2) ->
    let* s1 = translate_statement s1 in
    let+ s2 = translate_statement s2 in
    SEQUENCE (s1, s2)
  | Cabs.IF (cond, then_, else_) ->
    let* scondpre, cond, scondpost =
      translate_expression_strict "translate_statement (IF)" cond in
    let* then_ = translate_statement then_ in
    let+ else_ = translate_statement else_ in
    sequence [
      scondpre;
      IF (
        cond,
        sequence [scondpost; then_],
        sequence [scondpost; else_]);
    ]
  | Cabs.GOTO lbl -> Transl.return @@ GOTO lbl
  | _ ->
    let s = Utils.print_c Cprint.print_statement s in
    Transl.fail @@ Core_c_error (
      sprintf "Smallc.translate_statement: unsupported:\n\n%s\n" s)

(* Remove unnecessary casts from expressions. *)
let rec simpl_casts_exp : exp -> exp = function
  | UNARY (u, e, t) -> UNARY (u, simpl_casts_exp e, t)
  | BINARY (b, l, r, t) -> BINARY (b, simpl_casts_exp l, simpl_casts_exp r, t)
  | CAST (t, e) ->
    let e = simpl_casts_exp e in
    if equal_typ t @@ typeof e then e else CAST (t, e)
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

(* Find which vars are used *)

module Used = struct

  module Env = struct

    type t = String.Set.t

    let use (v : string) (env : t) : t = Set.add env v

  end

  include Monad.State.T1(Env)(Monad.Ident)
  include Monad.State.Make(Env)(Monad.Ident)

end

type 'a used = 'a Used.t

open Used.Let

let rec used_exp : exp -> unit used = function
  | UNARY (_, e, _) -> used_exp e
  | BINARY (_, l, r, _) ->
    let* () = used_exp l in
    used_exp r
  | CAST (_, e) -> used_exp e
  | CONST_INT _ -> Used.return ()
  | VARIABLE (v, _) -> Used.(update @@ Env.use @@ Theory.Var.name v)

and used_stmt : stmt -> unit used = function
  | NOP -> Used.return ()
  | BLOCK (_, s) -> used_stmt s
  | ASSIGN (_, e) -> used_exp e
  | CALL (f, args) | CALLASSIGN (_, f, args) ->
    let* () = used_exp f in
    Used.List.iter args ~f:used_exp
  | STORE (l, r) ->
    let* () = used_exp l in
    used_exp r
  | SEQUENCE (s1, s2) ->
    let* () = used_stmt s1 in
    used_stmt s2
  | IF (cond, st, sf) ->
    let* () = used_exp cond in
    let* () = used_stmt st in
    used_stmt sf
  | GOTO _ -> Used.return ()

let rec remove_unused_temps (used : String.Set.t) : stmt -> stmt = function
  | NOP -> NOP
  | BLOCK (tenv, s) -> BLOCK (tenv, remove_unused_temps used s)
  | ASSIGN ((v, _), _)
    when Theory.Var.is_virtual v
      && not (Set.mem used @@ Theory.Var.name v) -> NOP
  | ASSIGN _ as s -> s
  | (CALL _ | CALLASSIGN _) as s -> s
  | STORE _ as s -> s
  | SEQUENCE (s1, s2) ->
    SEQUENCE (remove_unused_temps used s1, remove_unused_temps used s2)
  | IF (cond, st, sf) ->
    IF (cond, remove_unused_temps used st, remove_unused_temps used sf)
  | GOTO _ as s -> s

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
  let used = Monad.State.exec (used_stmt s) String.Set.empty in
  let s = remove_unused_temps used s in
  let s = cleanup_nop_sequences s in
  (* Success! *)
  let prog = tenv, s in
  Events.send @@ Rule;
  Events.send @@ Info "Translated to the following SmallC program:";
  Events.send @@ Info "";
  Events.send @@ Info (to_string prog);
  Events.send @@ Rule;
  KB.return prog
