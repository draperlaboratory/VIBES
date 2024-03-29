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

open Core
open Bap.Std
open Bap_c.Std
open Monads.Std
open Bap_core_theory

module CT = Vibes_utils.Core_theory
module Utils = C_utils
module Log = Vibes_log.Stream
module Hvar = Vibes_higher_vars.Higher_var

(* Describes the data model used by the target.

   `sizes` describes the sizes of addresses and integers.
   `schar` describes whether `char` is signed or unsigned.
*)
module Data_model = struct

  type t = {
    sizes : C.Data.model;
    schar : bool;
  }

  let sizes (data : t) : C.Data.model = data.sizes
  let schar (data : t) : bool = data.schar

  (* Same across all data models. *)
  let char_size : int = 8 [@@warning "-32"]
  let short_size : int = 16 [@@warning "-32"]
  let long_long_size : int = 64 [@@warning "-32"]

  let int_size (data : t) : int = match data.sizes with
    | `LP32 -> 16
    | `ILP32 | `LLP64 | `LP64 -> 32
    | `ILP64 -> 64

  let long_size (data : t) : int = match data.sizes with
    | `LP32 | `ILP32 | `LLP64 -> 32
    | `ILP64 | `LP64 -> 64

end

module Size = struct

  type t = [`r8 | `r16 | `r32 | `r64]

  let of_int_exn : int -> t = function
    | 8 -> `r8
    | 16 -> `r16
    | 32 -> `r32
    | 64 -> `r64
    | n -> invalid_argf "Patch_c.size_of_int_exn: \
                         invalid size integer %d" n ()

end

type size = Size.t
type sign = SIGNED | UNSIGNED [@@deriving equal]
type typ = C.Type.t [@@deriving compare]

let equal_typ (a : typ) (b : typ) : bool = compare_typ a b = 0

let typ_sign (data : Data_model.t) : typ -> sign = function
  | `Basic {C.Type.Spec.t = `char; _} when data.schar -> SIGNED
  | `Basic {C.Type.Spec.t = `char; _} -> UNSIGNED
  | t when C.Type.is_signed t -> SIGNED
  | _ -> UNSIGNED

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

let int_typ (data : Data_model.t) (size : size) (sign : sign) : typ =
  let i32 s =
    if Data_model.int_size data = 32 then
      if s then `sint else `uint
    else if Data_model.long_size data = 32 then
      if s then `slong else `ulong
    else failwith "Data model doesn't support 32-bit" in
  let b = match size, sign with
    | `r8,  SIGNED   -> `schar
    | `r8,  UNSIGNED -> `uchar
    | `r16, SIGNED   -> `sshort
    | `r16, UNSIGNED -> `ushort     
    | `r32, SIGNED   -> i32 true
    | `r32, UNSIGNED -> i32 false
    | `r64, SIGNED   -> `slong_long
    | `r64, UNSIGNED -> `ulong_long in
  C.Type.basic b

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

and body = {
  tenv  : tenv;
  stmt  : stmt;
  label : string option;
} [@@deriving equal]

type t = {
  data  : Data_model.t;
  csize : C.Size.base;
  body  : body;
}

let label_env (t : t) : (String.Set.t, KB.conflict) result =
  let (let*) x f = Result.bind x ~f in
  let rec aux acc = function
    | NOP -> Ok acc
    | BLOCK {stmt; label; _} ->
      let* acc = match label with
        | Some l when Set.mem acc l ->
          let msg = Format.sprintf "Duplicate label %s" l in
          Error (Errors.Patch_c msg)
        | Some l -> Ok (Set.add acc l)
        | None -> Ok acc in
      aux acc stmt
    | ASSIGN _ -> Ok acc
    | CALL _ -> Ok acc
    | CALLASSIGN _ -> Ok acc
    | STORE _ -> Ok acc
    | SEQUENCE (s1, s2) ->
      let* acc = aux acc s1 in
      aux acc s2
    | IF (_, st, sf) ->
      let* acc = aux acc st in
      aux acc sf
    | GOTO _ -> Ok acc in
  aux String.Set.empty t.body.stmt

(* Helper for generating a sequence from a list of statements. *)
let sequence : stmt list -> stmt =
  List.fold_right ~init:NOP ~f:(fun s acc -> SEQUENCE (s, acc))

(* Translate back to FrontC representation so we can re-use their
   pretty-printers. *)
module Cabs = struct

  include Cabs

  let of_unop : unop -> unary_operator = function
    | MINUS  -> Cabs.MINUS
    | LNOT   -> Cabs.BNOT
    | MEMOF  -> Cabs.MEMOF
    | ADDROF -> Cabs.ADDROF

  let of_binop : binop -> binary_operator = function
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

  let rec of_exp : exp -> expression = function
    | UNARY (u, e, _) -> Cabs.(UNARY (of_unop u, of_exp e))
    | BINARY (b, e1, e2, _) ->
      Cabs.(BINARY (of_binop b, of_exp e1, of_exp e2))
    | CAST (t, e) -> Cabs.(CAST (Ctype.To_cabs.go t, of_exp e))
    | CONST_INT (i, _) ->
      Cabs.(CONSTANT (CONST_INT (Bitvec.to_string @@ Word.to_bitvec i)))
    | VARIABLE (v, _) -> Cabs.VARIABLE (Theory.Var.name v)

  and of_stmt : stmt -> statement = function
    | NOP -> Cabs.NOP
    | BLOCK {stmt; label = None; _} -> Cabs.BLOCK ([], of_stmt stmt)
    | BLOCK {stmt; label = Some l; _} -> Cabs.LABEL (l, of_stmt stmt)
    | ASSIGN ((v, _), e) ->
      Cabs.(
        COMPUTATION (
          BINARY (
            ASSIGN,
            VARIABLE (Theory.Var.name v),
            of_exp e)))
    | CALL (f, args) ->
      Cabs.(
        COMPUTATION (
          CALL (of_exp f, List.map args ~f:of_exp)))
    | CALLASSIGN ((v, _), f, args) ->
      Cabs.(
        COMPUTATION (
          BINARY (
            ASSIGN,
            VARIABLE (Theory.Var.name v),
            CALL (
              of_exp f,
              List.map args ~f:of_exp))))
    | STORE (addr, value) ->
      Cabs.(
        COMPUTATION (
          BINARY (
            ASSIGN,
            UNARY (MEMOF, of_exp addr),
            of_exp value)))
    | SEQUENCE (s1, s2) -> Cabs.SEQUENCE (of_stmt s1, of_stmt s2)
    | IF (cond, st, sf) ->
      Cabs.IF (of_exp cond, of_stmt st, of_stmt sf)
    | GOTO lbl -> Cabs.GOTO lbl

  (* Returns true if an expression is an l-value (i.e. can be mutated). *)
  let is_lvalue (e : expression) : bool =
    (* XXX: Seems like a hack. *)
    let rec aux ?(mem = false) = function
      (* We can use increment inside of a memory operation, while
         still having the expression result in an l-value. *)
      | Cabs.(UNARY (MEMOF, e))
      | Cabs.(INDEX (e, _))
      | Cabs.(MEMBEROF (e, _))
      | Cabs.(MEMBEROFPTR (e, _)) -> aux e ~mem:true
      (* Operand of increment must be an l-value regardless of
         whether we're inside of a MEMOF or not. *)
      | Cabs.(UNARY (POSINCR, e))
      | Cabs.(UNARY (PREINCR, e))
      | Cabs.(UNARY (POSDECR, e))
      | Cabs.(UNARY (PREDECR, e)) -> mem && aux e
      (* LHS of assign must be an l-value regardless of whether
         we're inside of a MEMOF or not. *)
      | Cabs.(BINARY (ASSIGN, e, _))
      | Cabs.(BINARY (ADD_ASSIGN, e, _))
      | Cabs.(BINARY (SUB_ASSIGN, e, _))
      | Cabs.(BINARY (MUL_ASSIGN, e, _))
      | Cabs.(BINARY (DIV_ASSIGN, e, _))
      | Cabs.(BINARY (MOD_ASSIGN, e, _))
      | Cabs.(BINARY (BAND_ASSIGN, e, _))
      | Cabs.(BINARY (BOR_ASSIGN, e, _))
      | Cabs.(BINARY (SHL_ASSIGN, e, _))
      | Cabs.(BINARY (SHR_ASSIGN, e, _)) -> aux e
      | Cabs.(VARIABLE _) -> true
      (* If we're inside a MEMOF, propagate that to the children. *)
      | Cabs.(QUESTION (_, l, r)) -> aux l ~mem && aux r ~mem
      | _ -> false in
    aux e

end

module Exp = struct

  type t = exp [@@deriving equal]

  let to_string (e : t) : string =
    Utils.print_c Cprint.print_statement @@ COMPUTATION (Cabs.of_exp e)

  (* Extract the embedded type of an expression. *)
  let typeof (data : Data_model.t) : t -> typ = function
    | UNARY (_, _, t) -> t
    | BINARY (_, _, _, t) -> t
    | CAST (t, _) -> t
    | CONST_INT (i, sign) ->
      let w = Word.bitwidth i in
      int_typ data (Size.of_int_exn w) sign
    | VARIABLE (_, t) -> t

  (* Convert to a particular type. *)
  let rec with_type
      (data : Data_model.t)
      (csize : C.Size.base)
      (e : t) (t : typ) : t = match e with
    | UNARY _ | BINARY _ | VARIABLE _ -> CAST (t, e)
    | CAST (t', e) -> begin
        match with_type data csize e t' with
        | CONST_INT _ as e -> with_type data csize e t
        | e -> CAST (t, e)
      end
    | CONST_INT (i, sign) -> begin 
        match t with
        | `Basic {C.Type.Spec.t = #C.Type.integer; _} ->
          let sign' = typ_sign data t in
          let sz = Option.value_exn (csize#bits t) in
          let ext = Bitvector.extract_exn in
          let i = match sign' with
            | UNSIGNED -> ext ~hi:Int.(sz - 1) i
            | SIGNED ->
              let i = match sign with
                | UNSIGNED -> Word.unsigned i
                | SIGNED -> Word.signed i in
              (* For some reason the extract operation always returns
                 an unsigned representation, but we want it to be signed. *)
              Word.signed @@ ext ~hi:Int.(sz - 1) i in
          CONST_INT (i, sign')
        | _ -> CAST (t, e)
      end

  let coerce_type
      (data : Data_model.t)
      (csize : C.Size.base)
      (e : t) (t : typ) : t = match e with
    | UNARY (u, e, _) -> UNARY (u, e, t)
    | BINARY (b, x, y, _) -> BINARY (b, x, y, t)
    | CAST (_, e) -> CAST (t, e)
    | CONST_INT _ -> with_type data csize e t
    | VARIABLE (v, _) -> VARIABLE (v, t)

end

module Stmt = struct

  type t = stmt [@@deriving equal]

  let to_string (s : t) : string =
    Utils.print_c Cprint.print_statement @@ Cabs.of_stmt s

end

module Type = struct

  type t = typ [@@deriving equal]

  let to_string : t -> string = Format.asprintf "%a" C.Type.pp
  let sign (data : Data_model.t) (t : t) : sign = typ_sign data t

  (* See whether the elements of the pointer te can unify. *)
  let rec unify_ptr (t1 : t) (t2 : t) : t option =
    match t1, t2 with
    | `Pointer {C.Type.Spec.t = t1; _},
      `Pointer {C.Type.Spec.t = t2; _} ->
      Option.(unify_ptr t1 t2 >>| C.Type.pointer)
    | `Void, _ -> Some t2
    | _, `Void -> Some t1
    | _ -> if equal t1 t2 then Some t1 else None

  (* Perform the conversions for pure expressions. *)
  let unify
      (data : Data_model.t)
      (csize : C.Size.base)
      (t1 : t)
      (t2 : t) : t option =
    match t1, t2 with
    | `Void, `Void -> Some `Void
    | `Void, _ | _, `Void -> None
    | `Basic {C.Type.Spec.t = #C.Type.integer; _},
      `Pointer _
    | `Pointer _,
      `Basic {C.Type.Spec.t = #C.Type.integer; _} -> None
    | _, `Function _ | `Function _, _ -> None
    | `Pointer {C.Type.Spec.t = t1'; _},
      `Pointer {C.Type.Spec.t = t2'; _} ->
      Option.(unify_ptr t1' t2' >>| C.Type.pointer)
    | `Basic {C.Type.Spec.t = #C.Type.integer; _},
      `Basic {C.Type.Spec.t = #C.Type.integer; _} ->
      let size1 = Option.value_exn (csize#bits t1) in
      let size2 = Option.value_exn (csize#bits t2) in
      begin match compare size1 size2 with
        | n when n < 0 -> Some t2
        | n when n > 0 -> Some t1
        | _ -> match sign data t1, sign data t2 with
          | SIGNED, UNSIGNED | UNSIGNED, SIGNED ->
            Some (int_typ data (Size.of_int_exn size1) UNSIGNED)
          | _ -> Some t1
      end
    | _ -> None

  (* Unify pointer types to integers. *)
  let rec unify_ptr_to_int
      (data : Data_model.t)
      (csize : C.Size.base)
      (bits : int)
      (t1 : t) (t2 : t) : t option = match t1, t2 with
    | `Pointer _, `Basic {C.Type.Spec.t = #C.Type.integer; _}
    | `Basic {C.Type.Spec.t = #C.Type.integer; _}, `Pointer _
    | `Pointer _, `Pointer _ ->
      Some (int_typ data (Size.of_int_exn bits) UNSIGNED)
    | (`Basic {C.Type.Spec.t = #C.Type.integer; _} | `Pointer _),
      `Function {C.Type.Spec.t = {C.Type.Proto.return; _}; _} ->
      unify_ptr_to_int data csize bits t1 return
    | `Function {C.Type.Spec.t = {C.Type.Proto.return; _}; _},
      (`Basic {C.Type.Spec.t = #C.Type.integer; _} | `Pointer _) ->
      unify_ptr_to_int data csize bits return t2
    | `Basic {C.Type.Spec.t = #C.Type.integer; _},
      `Basic {C.Type.Spec.t = #C.Type.integer; _} ->
      unify data csize t1 t2
    | _ -> None

  (* Same as `unify_ptr` but favor the lhs. *)
  let rec cast_ptr_assign (t1 : t) (t2 : typ) : typ option =
    match t1, t2 with
    | `Pointer {C.Type.Spec.t = t1; _},
      `Pointer {C.Type.Spec.t = t2; _} ->
      cast_ptr_assign t1 t2
    | `Void, _ | _, `Void -> Some t1
    | _ -> Option.some_if (equal t1 t2) t1

  (* Perform the conversions for an assignment. Returns the unified type
     and the expression with an explicit cast. *)
  let cast_assign
      (data : Data_model.t)
      (csize : C.Size.base)
      (tl : t) (tr : typ)
      (r : exp) : (typ * exp) option =
    match tl, tr with
    | `Void, _ | _, `Void -> None
    | `Basic {C.Type.Spec.t = #C.Type.integer; _}, `Pointer _
    | `Pointer _, `Basic {C.Type.Spec.t = #C.Type.integer; _} -> None
    | `Function {C.Type.Spec.t = pl; _},
      `Function {C.Type.Spec.t = pr; _} -> begin
        match unify data csize pl.return pr.return with
        | None -> None
        | Some return -> Option.try_with @@ fun () ->
          let args =
            List.zip_exn pl.args pr.args |>
            List.map ~f:(fun ((nl, l), (_nr, r)) ->
                nl, Option.value_exn (unify data csize l r)) in
          C.Type.function_ ~return args, r
      end
    | `Function _, _ | _, `Function _ -> None
    | `Pointer {C.Type.Spec.t = t1'; _},
      `Pointer {C.Type.Spec.t = t2'; _} ->
      cast_ptr_assign t1' t2' |> Option.map ~f:(fun _ ->
          tl, Exp.with_type data csize r tl)
    | `Basic {C.Type.Spec.t = #C.Type.integer; _},
      `Basic {C.Type.Spec.t = #C.Type.integer; _} ->
      let sizel = Option.value_exn (csize#bits tl) in
      let sizer = Option.value_exn (csize#bits tr) in
      if sizel = sizer && equal_sign (sign data tl) (sign data tr)
      then Some (tl, r) else Some (tl, Exp.with_type data csize r tl)
    | _ -> None

  let field_offset
      (csize : C.Size.base)
      (name : string)
      (t : t) : word option =
    match C.Abi.layout csize t with
    | {layout = C.Data.Seq data} ->
      let init = 0 and finish _ = None in
      let width = csize#pointer |> Bap.Std.Size.in_bits in
      List.fold_until data ~init ~finish ~f:(fun acc -> function
          | Imm (_, Field (f, _)) when String.(name = f) -> Stop (Some acc)
          | Imm (s, _) -> Continue (acc + s)
          | _ -> Stop None) |>
      Option.map ~f:(fun off -> Word.of_int (off lsr 3) ~width)
    | _ -> None

end

let to_string (prog : t) : string =
  let vars =
    Map.to_alist prog.body.tenv |> List.map ~f:(fun (v, t) ->
        sprintf "%s %s;" (Type.to_string t) v) |>
    String.concat ~sep:"\n" in
  let stmt = Cabs.of_stmt prog.body.stmt in
  let stmt =
    Utils.print_c Cprint.print_statement @@
    match prog.body.label with
    | Some l -> Cabs.LABEL (l, stmt)
    | None -> stmt in
  Format.sprintf "%s\n%s" vars stmt

(* Optimization and simplification passes. *)
module Opt = struct

  (* The elaboration will leave a bunch of nops in the AST which makes
     pretty-printing quite ugly. This pass removes them. *)
  module Nops = struct

    module Env = struct

      type t = String.Set.t

      let use (v : string) (env : t) : t = Set.add env v

    end

    include Monad.State.T1(Env)(Monad.Ident)
    include Monad.State.Make(Env)(Monad.Ident)

    let rec collect_label : stmt -> unit t = function
      | NOP -> return ()
      | BLOCK {stmt; _} -> collect_label stmt
      | ASSIGN _ -> return ()
      | CALL _ -> return ()
      | CALLASSIGN _ -> return ()
      | STORE _ -> return ()
      | SEQUENCE (s1, s2) ->
        let* () = collect_label s1 in
        collect_label s2
      | IF (_, st, sf) ->
        let* () = collect_label st in
        collect_label sf
      | GOTO l -> update @@ Env.use l

    let go (s : stmt) : stmt =
      let rec aux used s = match s with
        | NOP -> NOP
        | BLOCK {tenv; stmt; label} -> begin
            match aux used stmt with
            | NOP -> begin
                match label with
                | Some l when Set.mem used l ->
                  BLOCK {tenv; stmt = NOP; label}
                | Some _ | None -> NOP
              end
            | stmt -> BLOCK {tenv; stmt; label}
          end
        | ASSIGN ((v1, _), VARIABLE (v2, _)) when Theory.Var.Top.(v1 = v2) -> NOP
        | ASSIGN _ -> s
        | CALL _ -> s
        | CALLASSIGN _ -> s
        | STORE _ -> s
        | SEQUENCE (NOP, s) -> aux used s
        | SEQUENCE (s, NOP) -> aux used s
        | SEQUENCE (s1, s2) -> begin
            let s1 = aux used s1 in
            let s2 = aux used s2 in
            match s1, s2 with
            | NOP, _ -> s2
            | _, NOP -> s1
            | _ -> SEQUENCE (s1, s2)
          end
        | IF (cond, st, sf) -> begin
            let st = aux used st in
            let sf = aux used sf in
            match st, sf with
            | NOP, NOP -> NOP
            | _ -> IF (cond, st, sf)
          end
        | GOTO _ -> s in
      let used = Monad.State.exec (collect_label s) String.Set.empty in
      aux used s

  end

  module Cast = struct

    (* Remove unnecessary casts from expressions. *)
    let rec exp (data : Data_model.t) : exp -> exp = function
      | UNARY (u, e, t) -> UNARY (u, exp data e, t)
      | BINARY (b, l, r, t) ->
        BINARY (b, exp data l, exp data r, t)
      | CAST (t, e) ->
        let e = exp data e in
        if equal_typ t @@ Exp.typeof data e then e else CAST (t, e)
      | (CONST_INT _ | VARIABLE _) as e -> e

    and go (data : Data_model.t) : stmt -> stmt = function
      | NOP -> NOP
      | BLOCK {tenv; stmt; label} -> BLOCK {tenv; stmt = go data stmt; label}
      | ASSIGN (v, e) -> ASSIGN (v, exp data e)
      | CALL (f, args) -> CALL (exp data f, List.map args ~f:(exp data))
      | CALLASSIGN (v, f, args) ->
        CALLASSIGN (v, exp data f, List.map args ~f:(exp data))
      | STORE (l, r) -> STORE (exp data l, exp data r)
      | SEQUENCE (s1, s2) -> SEQUENCE (go data s1, go data s2)
      | IF (cond, st, sf) -> IF (exp data cond, go data st, go data sf)
      | GOTO _ as s -> s

  end

  (* Find which vars are used, and remove assignments to unused temps. *)
  module Unused = struct

    module Env = struct

      type t = String.Set.t

      let use (v : string) (env : t) : t = Set.add env v

    end

    include Monad.State.T1(Env)(Monad.Ident)
    include Monad.State.Make(Env)(Monad.Ident)

    let rec collect_exp : exp -> unit t = function
      | UNARY (_, e, _) -> collect_exp e
      | BINARY (_, l, r, _) ->
        let* () = collect_exp l in
        collect_exp r
      | CAST (_, e) -> collect_exp e
      | CONST_INT _ -> return ()
      | VARIABLE (v, _) -> update @@ Env.use @@ Theory.Var.name v

    and collect_stmt : stmt -> unit t = function
      | NOP -> return ()
      | BLOCK {stmt; _} -> collect_stmt stmt
      | ASSIGN (_, e) -> collect_exp e
      | CALL (f, args) | CALLASSIGN (_, f, args) ->
        let* () = collect_exp f in
        List.iter args ~f:collect_exp
      | STORE (l, r) ->
        let* () = collect_exp l in
        collect_exp r
      | SEQUENCE (s1, s2) ->
        let* () = collect_stmt s1 in
        collect_stmt s2
      | IF (cond, st, sf) ->
        let* () = collect_exp cond in
        let* () = collect_stmt st in
        collect_stmt sf
      | GOTO _ -> return ()

    let remove (s : stmt) : stmt =
      let rec aux used = function
        | NOP -> NOP
        | BLOCK {tenv; stmt; label} ->
          BLOCK {tenv; stmt = aux used stmt; label}
        | ASSIGN ((v, _), _)
          when Theory.Var.is_virtual v
            && not (Set.mem used @@ Theory.Var.name v) -> NOP
        | ASSIGN _ as s -> s
        | (CALL _ | CALLASSIGN _) as s -> s
        | STORE _ as s -> s
        | SEQUENCE (s1, s2) ->
          SEQUENCE (aux used s1, aux used s2)
        | IF (cond, st, sf) ->
          IF (cond, aux used st, aux used sf)
        | GOTO _ as s -> s in
      let used = Monad.State.exec (collect_stmt s) String.Set.empty in
      aux used s

  end

end

(* Translation monad. *)
module Transl = struct

  module Env = struct

    type t = {
      target : Theory.target;
      data   : Data_model.t;
      csize  : C.Size.base;
      tenv   : tenv;
      gamma  : tenv;
      tags   : tenv;
    }

    let initial_gamma (data : Data_model.t) : tenv =
      List.map Parse_c.builtin_typenames ~f:(fun b ->
          let size = Size.of_int_exn b.size in
          let sign = if b.signed then SIGNED else UNSIGNED in
          b.name, int_typ data size sign) |>
      String.Map.of_alist_exn

    let create
        ~(target : Theory.target)
        ~(data : Data_model.t)
        ~(csize : C.Size.base)
        () = {
      target;
      data;
      csize;
      tenv = String.Map.empty;
      gamma = initial_gamma data;
      tags = String.Map.empty;
    }

    let typeof (var : string) (env : t) : typ option =
      Map.find env.tenv var

  end

  include Monad.State.T1(Env)(KB)
  include Monad.State.Make(Env)(KB)

  let fail (msg : string) : 'a t = lift @@ KB.fail @@ Errors.Patch_c msg

  let gamma : Ctype.gamma t =
    let+ {gamma; _} = get () in
    fun name -> Map.find gamma name |> Option.value ~default:`Void

  let tag : Ctype.tag t =
    let+ {tags; _} = get () in Ctype.{
        lookup = fun what name -> match Map.find tags name with
          | None -> what name []
          | Some t -> t
      }

  let add_type (key : string) (data : typ) : unit t =
    update @@ fun env -> {
      env with gamma = Map.set env.gamma ~key ~data;
    }

  let add_tag (key : string) (data : typ) : unit t =
    update @@ fun env -> {
      env with tags = Map.set env.tags ~key ~data;
    }

end

(* The main pass for elaboration and typechecking *)
module Main = struct

  open Transl.Let

  let return = Transl.return
  let get = Transl.get
  let gets = Transl.gets
  let update = Transl.update
  let lift = Transl.lift
  let fail = Transl.fail

  type 'a transl = 'a Transl.t

  (* Create a fresh temporary variable. *)
  let new_tmp (t : typ) : var transl =
    let* {csize; _} = get () in
    let* v = match csize#bits t with
      | None ->
        let s = Bap.Std.Type.sort Unk in
        let+ v = lift @@ Theory.Var.fresh s in
        Theory.Var.forget v
      | Some size ->
        let s = Theory.Bitv.define size in
        let+ v = lift @@ Theory.Var.fresh s in
        Theory.Var.forget v in
    let key = Theory.Var.name v in
    let+ () = update @@ fun env -> {
        env with tenv = Map.set env.tenv ~key ~data:t;
      } in
    v, t

  let unsupported_typ
      ?(msg : string = "")
      (name : string)
      (t : typ) : _ transl =
    let msg =
      if String.is_empty msg then
        Format.asprintf "%s type %a is unsupported"
          name C.Type.pp t
      else
        Format.asprintf "%s type %a is unsupported:\n%s"
          name C.Type.pp t msg in
    fail msg

  let check_no_cv
      ?(msg : string = "")
      (name : string)
      (t : typ)
      (cv : C.Type.cv C.Type.qualifier) : unit transl =
    if not (cv.const || cv.volatile) then return ()
    else unsupported_typ name t ~msg

  let check_no_cvr
      ?(msg : string = "")
      (name : string)
      (t : typ)
      (cvr : C.Type.cvr C.Type.qualifier) : unit transl =
    if not (cvr.const || cvr.volatile || cvr.restrict) then return ()
    else unsupported_typ name t ~msg

  let check_no_attrs
      ?(msg : string = "")
      (name : string)
      (t : typ)
      (attrs : C.Type.attr list) : unit transl =
    if List.is_empty attrs then return ()
    else unsupported_typ name t ~msg

  (* Disallow use of types that our compiler doesn't support yet. *)
  let rec check_supported_typ ?(msg : string = "") : typ -> unit transl = function
    | `Void -> return ()
    | `Basic {C.Type.Spec.qualifier; t = #C.Type.integer; attrs} as t ->
      let* () = check_no_cv "Integer" t qualifier ~msg in
      check_no_attrs "Integer" t attrs ~msg
    | `Basic {C.Type.Spec.t = #C.Type.floating; _} as t ->
      unsupported_typ "Floating point" t ~msg
    | `Pointer {C.Type.Spec.qualifier; t; attrs} ->
      let* () = check_supported_typ t ~msg in
      let* () = check_no_cvr "Pointer" t qualifier ~msg in
      check_no_attrs "Pointer" t attrs ~msg
    | `Array _ as t -> unsupported_typ "Array" t ~msg
    | `Structure {C.Type.Spec.t = compound; attrs; _} as t ->
      let* () = Transl.List.iter compound.fields ~f:(fun (_, t) ->
          check_supported_typ t ~msg) in
      check_no_attrs "Structure" t attrs ~msg
    | `Union _ as t -> unsupported_typ "Union" t ~msg
    | `Function {C.Type.Spec.t = proto; _} as t when proto.variadic ->
      unsupported_typ "Variadic function" t ~msg
    | `Function {C.Type.Spec.t = proto; attrs; _} as t ->
      let* () = check_supported_typ proto.return ~msg in
      let* () =
        List.map proto.args ~f:snd |>
        Transl.List.iter ~f:(check_supported_typ ~msg) in
      check_no_attrs "Function" t attrs ~msg

  (* Translate a base type. *)
  let go_type
      ?(msg : string = "")
      (t : Cabs.base_type) : typ transl =
    let* gamma = Transl.gamma and* tag = Transl.tag in
    let t = Ctype.ctype gamma tag t in
    let+ () = check_supported_typ t ~msg in
    t

  let typ_unify_error : 'a. Cabs.expression -> typ -> typ -> 'a transl =
    fun e t1 t2 ->
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    let msg = Format.asprintf
        "Failed to unify types %a and %a in expression:\n\n%s\n"
        C.Type.pp t1 C.Type.pp t2 s in
    fail msg

  let typ_error (e : Cabs.expression) (t : typ) (msg : string) : 'a transl =
    let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
    let msg = Format.asprintf
        "Expression:\n\n%s\n\nunified to type %a. %s\n"
        s C.Type.pp t msg in
    fail msg

  (* An elaborated expression. *)
  type eexp = stmt * exp option * stmt

  (* An elaborated expression that must have some result. *)
  type eexp_strict = stmt * exp * stmt

  (* Helper functions used during translation. *)
  module Helper = struct

    (* If the expression had side-effects, then store the result in a
       temporary variable. *)
    let new_tmp_or_simple
        (pre : stmt)
        (e : exp)
        (post : stmt) : (exp, eexp_strict) Either.t transl =
      let pre = Opt.Nops.go pre in
      let post = Opt.Nops.go post in
      match pre, post with
      | NOP, NOP -> return @@ First e
      | _, NOP -> return @@ Second (pre, e, post)
      | _ ->
        let* {data; _} = get () in
        let+ tmp = new_tmp @@ Exp.typeof data e in
        let pre = sequence [pre; ASSIGN (tmp, e)] in
        Second (pre, VARIABLE tmp, post)

    (* Helper for binary operators where the sequencing of side-effects
       for the operands is important. *)
    let binary_tmp_or_simple :
      'op. 'op ->
      stmt -> exp -> stmt ->
      stmt -> exp -> stmt ->
      typ ->
      f:('op -> exp -> exp -> typ -> exp transl) ->
      eexp transl = fun op pre1 e1 post1 pre2 e2 post2 t ~f ->
      (* Evaluate left to right. *)
      let* te1 = new_tmp_or_simple pre1 e1 post1 in
      let* te2 = new_tmp_or_simple pre2 e2 post2 in
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

    let increment_rvalue_error (e : Cabs.expression) (t : typ) : 'a transl =
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      let msg = Format.asprintf
          "Patch_c.increment: expression:\n\n%s\n\nhas type %a, which \
           is not an l-value" s C.Type.pp t in
      fail msg

    (* Increment value based on the type. *)
    let increment (e : Cabs.expression) (t : typ) : exp transl =
      let* width = gets @@ fun {target; _} ->
        Theory.Target.data_addr_size target in
      match t with
      | `Pointer {C.Type.Spec.t = t'; _} -> begin
          match t' with
          | `Basic {C.Type.Spec.t = b; _} ->
            let+ {csize; _} = get () in
            let size = csize#basic b in
            let i = Word.of_int ~width @@ Bap.Std.Size.in_bytes size in
            CONST_INT (i, UNSIGNED)
          | `Void | `Pointer _ | `Function _ ->
            (* Pointer to a pointer: use the word size. *)
            let i = Word.of_int ~width (width lsr 3) in
            return @@ CONST_INT (i, UNSIGNED)
          | _ -> assert false
        end
      | `Basic {C.Type.Spec.t = #C.Type.integer; _} ->
        let+ {data; _} = get () in
        let sign = Type.sign data t in
        let i = Word.one width in
        let i = if equal_sign sign SIGNED then Word.signed i else i in
        CONST_INT (i, sign)
      | _ -> increment_rvalue_error e t

    (* Do type checking and either generate an assignment or a store. *)
    let make_assign
        ?(is_store : bool = false)
        ?(e : Cabs.expression = NOTHING)
        (e1 : exp) (e2 : exp) : stmt transl =
      let* {data; csize; _} = get () in
      let t1 = Exp.typeof data e1 in
      let t2 = Exp.typeof data e2 in
      match Type.cast_assign data csize t1 t2 e2 with
      | None ->
        let t1 = if is_store then C.Type.pointer t1 else t1 in
        typ_unify_error e t1 t2
      | Some (_, e2) -> match e1 with
        | VARIABLE var -> return @@ ASSIGN (var, e2)
        | UNARY (MEMOF, addr, _) -> return @@ STORE (addr, e2)
        | _ -> fail "Patch_c.make_assign: unexpected shape"

    (* Generate an arithmetic expression depending on whether pointer
       arithmetic is allowed. *)
    let make_arith
        ?(no_ptr : bool = false)
        ?(e : Cabs.expression = Cabs.NOTHING)
        (b : binop) (e1 : exp) (e2 : exp) : exp transl =
      let* {data; csize; _} = get () in
      let t1 = Exp.typeof data e1 in
      let t2 = Exp.typeof data e2 in
      match t1, t2 with
      | `Pointer {C.Type.Spec.t = `Void; _}, _
      | _, `Pointer {C.Type.Spec.t = `Void; _} ->
        typ_unify_error e t1 t2
      | (`Pointer _, _ | _, `Pointer _) when no_ptr ->
        typ_unify_error e t1 t2
      | `Pointer _, `Pointer _ ->
        typ_unify_error e t1 t2
      | _ ->
        let+ t, e1, e2 = match t1, t2 with
          | `Pointer _, `Basic {C.Type.Spec.t = #C.Type.integer; _} ->
            let+ inc = increment NOTHING t1 in
            t1, e1, BINARY (MUL, e2, inc, t2)
          | `Basic {C.Type.Spec.t = #C.Type.integer; _}, `Pointer _ ->
            let+ inc = increment NOTHING t2 in
            t2, BINARY (MUL, e1, inc, t1), e2
          | `Basic {C.Type.Spec.t = #C.Type.integer; _},
            `Basic {C.Type.Spec.t = #C.Type.integer; _} -> begin
              (* The size of a constant integer is ambiguous until
                 we use it in some kind of operation. *)
              match e1, e2 with
              | CONST_INT _, _ -> return (t2, Exp.with_type data csize e1 t2, e2)
              | _, CONST_INT _ -> return (t1, e1, Exp.with_type data csize e2 t1)
              | _ -> begin
                  match Type.unify data csize t1 t2 with
                  | None -> typ_unify_error e t1 t2
                  | Some t ->
                    let e1 = Exp.with_type data csize e1 t in
                    let e2 = Exp.with_type data csize e2 t in
                    return (t, e1, e2)
                end
            end
          | _ -> typ_unify_error e t1 t2 in
        BINARY (b, e1, e2, t)

  end

  (* Resolve the names of struct and union types. *)
  let resolver lookup = object(self)
    inherit [unit] C.Type.Mapper.base

    method! map_union = self#resolve
    method! map_structure = self#resolve

    method private resolve t = match t with
      | {C.Type.Compound.fields = []; _} -> self#lookup t
      | _ -> t

    method private lookup {C.Type.Compound.fields; name} =
      match lookup name with
      | Some `Structure {C.Type.Spec.t; _}
      | Some `Union {C.Type.Spec.t; _} -> C.Type.Compound.{t with name}
      | _ -> {C.Type.Compound.fields; name}
  end

  (* Translate a scoped statement. *)
  let rec go_body ((defs, stmt) : Cabs.body) : body transl =
    let* {tenv; gamma; tags; csize; _} = get () in
    let* new_tenv, inits =
      Transl.List.fold defs ~init:(tenv, [])
        ~f:(fun (tenv, inits) -> function
            | DECDEF (_t, _storage, names) ->
              Transl.List.fold names ~init:(tenv, inits)
                ~f:(fun (tenv, inits) (name, t, _attrs, e) ->
                    let* t = go_type t in
                    let+ () = Transl.add_type name t in
                    let size = Option.value_exn (csize#bits t) in
                    let s = Theory.Bitv.define size in
                    let tenv = Map.set tenv ~key:name ~data:t in
                    let v = Theory.Var.define s name in
                    tenv, ((Theory.Var.forget v, t), e) :: inits)
            | TYPEDEF ((_t, _storage, names), _attrs) ->
              let+ () =
                Transl.List.iter names ~f:(fun (name, t, _attrs, _e) ->
                    let* t = go_type t in
                    Transl.add_type name t) in
              tenv, inits
            | ONLYTYPEDEF (t, _storage, names) ->
              let* () = match t with
                | STRUCT (name, _) | UNION (name, _) | ENUM (name, _) ->
                  let* t = go_type t in
                  Transl.add_tag name t
                | _ -> return () in
              let+ () =
                Transl.List.iter names ~f:(fun (name, t, _attrs, _e) ->
                    let* t = go_type t in
                    Transl.add_type name t) in
              tenv, inits
            | def ->
              let msg = Format.sprintf
                  "Patch_c.go_body: unsupported definition:\n\n%s\n\n" @@
                Utils.print_c Cprint.print_def def in
              fail msg) in
    let* inits = go_inits @@ List.rev inits in
    let* () = update @@ fun env ->
      let resolve = (resolver @@ Map.find env.tags)#run in
      {env with tenv = new_tenv; gamma = Map.map env.gamma ~f:resolve} in
    let* s = go_statement stmt in
    let+ () = update @@ fun env -> {env with tenv; gamma; tags} in
    {tenv = new_tenv; stmt = SEQUENCE (inits, s); label = None}

  (* Initialize the declared variables. *)
  and go_inits (inits : (var * Cabs.expression) list) : stmt transl =
    Transl.List.fold inits ~init:NOP ~f:(fun acc (v, e) ->
        let+ spre, e', spost = go_expression e in
        match e' with
        | None -> sequence [acc; spre; spost]
        | Some e' -> sequence [acc; spre; ASSIGN (v, e'); spost])

  (* Translate an expression which may be `None`. Also returns any side effects
     produced by the expression which are required to compute its result.

     We separate effects that happen before the expression is evaluated, and
     effects that happen after. This is solely to handle POSINCR/POSDECR
     operators.

     `assign` denotes whether we want the result of evaluating this expression
     to be assigned to a particular variable. Otherwise, a fresh temporary is
     created to hold the result

     `computation` denotes whether this expression was derived from a FrontC
     COMPUTATION statement. This means that the expression is being evaluated
     for its side effects only, and the result may then be discarded.
  *)
  and go_expression
      ?(assign : var option = None)
      ?(computation = false)
      (e : Cabs.expression) : eexp transl =
    match e with
    | Cabs.NOTHING -> return (NOP, None, NOP)
    | Cabs.UNARY (u, e) ->
      let+ spre, e, spost = go_unary_operator u e in
      if computation then spre, None, spost else spre, e, spost
    | Cabs.BINARY (b, lhs, rhs) ->
      let+ spre, e, spost = go_binary_operator b lhs rhs in
      if computation then spre, None, spost else spre, e, spost
    | Cabs.QUESTION (cond, then_, else_) ->
      go_question cond then_ else_ ~assign
    | Cabs.CAST (t, e) ->
      let* t = go_type t in
      let+ spre, e', spost =
        go_expression_strict "go_expression (CAST)" e ~assign in
      if computation then spre, None, spost
      else spre, Some (CAST (t, e')), spost
    | Cabs.CALL (f, args) ->
      let+ s, e = go_call f args ~assign ~computation in
      s, e, NOP
    | Cabs.COMMA [] -> return (NOP, None, NOP)
    | Cabs.COMMA exps ->
      (* Guarded by the pattern match above. *)
      let e = List.last_exn exps in
      let* stmts =
        List.drop_last_exn exps |>
        List.map ~f:(fun e -> Cabs.COMPUTATION e) |>
        Transl.List.map ~f:go_statement in
      let+ pre, e, post = go_expression e ~assign in
      sequence (stmts @ [pre]), e, post
    | Cabs.CONSTANT _ when computation -> return (NOP, None, NOP)
    | Cabs.(CONSTANT (CONST_INT s)) ->
      let i = Int64.of_string s in
      (* Try to fit it in the word size of the target machine, otherwise
         default to 64-bit. *)
      let+ {target; _} = get () in
      let width = Theory.Target.bits target in
      let width =
        if width = 64 then width
        else let open Int64 in
          (* Pad the upper `64 - width` bits with ones. *)
          let padding = (-1L) lsl Int.(64 - width) in
          let wm1 = Int.(width - 1) in
          let min_val_signed = padding lor (1L lsl wm1) in
          let max_val_signed = (1L lsl wm1) - 1L in
          let min_val_unsigned = 0L in
          let max_val_unsigned = (1L lsl width) - 1L in
          if (i >= min_val_signed && i <= max_val_signed)
          || (i >= min_val_unsigned && i <= max_val_unsigned)
          then width else 64 in
      let i = Word.(signed @@ of_int64 ~width i) in
      NOP, Some (CONST_INT (i, SIGNED)), NOP
    | Cabs.(CONSTANT (CONST_CHAR s)) ->
      let+ sign = gets @@ fun {data; _} ->
        if data.schar then SIGNED else UNSIGNED in
      let i = Word.of_int ~width:8 Char.(to_int @@ of_string s) in
      let i = Word.signed i in
      NOP, Some (CONST_INT (i, sign)), NOP
    | Cabs.VARIABLE v -> begin
        let* t = Transl.(gets @@ Env.typeof v) in
        match t with
        | Some t ->
          let+ {csize; _} = get () in
          let size = Option.value_exn (csize#bits t) in
          let s = Theory.Bitv.define size in
          let v = Theory.Var.define s v |> Theory.Var.forget in
          let e = if computation then None else Some (VARIABLE (v, t)) in
          NOP, e, NOP
        | None ->
          fail (
            sprintf "Patch_c.go_expression: undeclared variable %s\n" v)
      end
    | Cabs.EXPR_SIZEOF _ when computation -> return (NOP, None, NOP)
    | Cabs.EXPR_SIZEOF e ->
      let* _, e, _ =
        go_expression_strict "go_expression (EXPR_SIZEOF)" e in
      let+ {target; data; csize; _} = get () in
      let width = Theory.Target.data_addr_size target in
      let t = Exp.typeof data e in
      let size = Option.value_exn (csize#bits t) in
      NOP, Some (CONST_INT (Word.of_int ~width (size lsr 3), UNSIGNED)), NOP
    | Cabs.TYPE_SIZEOF t ->
      let s = Utils.print_c Cprint.print_base_type t in
      let* t = go_type t ~msg:(sprintf "go_expression (TYPE_SIZEOF) %s: " s) in
      let+ {target; csize; _} = get () in
      let width = Theory.Target.data_addr_size target in
      let size = Option.value_exn (csize#bits t) in
      NOP, Some (CONST_INT (Word.of_int ~width (size lsr 3), UNSIGNED)), NOP
    | Cabs.INDEX (ptr, idx) ->
      let+ (spre, e, spost), t = go_index ptr idx in
      begin match e with
        | None -> spre, None, spost
        | Some e ->
          let e = UNARY (MEMOF, CAST (C.Type.pointer t, e), t) in
          spre, Some e, spost
      end
    | Cabs.MEMBEROF (ptr, field) ->
      (* TODO: if the entire struct fits inside of a register, then can we
         optimize this to just use bitwise operations to extract the
         corresponding element? *)
      let ptr = Cabs.UNARY (ADDROF, ptr) in
      go_memberof ptr field
    | Cabs.MEMBEROFPTR (ptr, field) -> go_memberof ptr field
    | _ ->
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      fail (
        sprintf "Patch_c.go_expression: unsupported:\n\n%s\n" s)

  (* Translate an expression and expect that the value is not `None`. *)
  and go_expression_strict
      ?(assign : var option = None)
      (stage : string)
      (e : Cabs.expression) : eexp_strict transl =
    let* spre, e', spost = go_expression e ~assign in
    match e' with
    | Some e' -> return (spre, e', spost)
    | None ->
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      fail (
        sprintf "Patch_c.%s: invalid expression:\n\n%s\n" stage s)

  (* Translate an expression that must be an l-value. *)
  and go_expression_lvalue
      ?(assign : var option = None)
      (stage : string)
      (e : Cabs.expression) : eexp_strict transl =
    if Cabs.is_lvalue e
    then go_expression_strict stage e ~assign
    else
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      fail (
        sprintf "Patch_c.%s: expression:\n\n%s\n\nis not an l-value" stage s)

  (* Translate unary operators. *)
  and go_unary_operator
      (u : Cabs.unary_operator)
      (e : Cabs.expression) : eexp transl =
    let lval = go_expression_lvalue "go_unary_operator" in
    let exp = go_expression_strict "go_unary_operator" in
    let* {data; csize; _} = get () in
    match u with
    | Cabs.MINUS ->
      let+ spre, e, spost = exp e in
      spre, Some (UNARY (MINUS, e, Exp.typeof data e)), spost
    | Cabs.PLUS ->
      let+ spre, e, spost = exp e in
      spre, Some e, spost
    | Cabs.NOT ->
      let+ spre, e, spost = exp e in
      let t = Exp.typeof data e in
      let width = Option.value_exn (csize#bits t) in
      let i = Word.zero width in
      spre, Some (BINARY (EQ, e, CONST_INT (i, UNSIGNED), t)), spost
    | Cabs.BNOT ->
      let+ spre, e, spost = exp e in
      spre, Some (UNARY (LNOT, e, Exp.typeof data e)), spost
    | Cabs.MEMOF -> begin
        let* spre, e', spost = exp e in
        match Exp.typeof data e' with
        | `Pointer {C.Type.Spec.t = `Void; _} ->
          let s =
            Utils.print_c Cprint.print_statement
              Cabs.(COMPUTATION (UNARY (u, e))) in
          fail (
            sprintf "Patch_c.go_unary_operator: in expression\
                     \n\n%s\n\ncannot dereference a value of type void*" s)
        | `Pointer {C.Type.Spec.t; _} ->
          return (spre, Some (UNARY (MEMOF, e', t)), spost)
        | _ ->
          let s =
            Utils.print_c Cprint.print_statement
              Cabs.(COMPUTATION (UNARY (u, e))) in
          fail (
            sprintf "Patch_c.go_unary_operator: expected pointer type \
                     for operand of expression:\n\n%s\n" s)
      end
    | Cabs.ADDROF ->
      let+ spre, e', spost = lval e in
      let t = C.Type.pointer (Exp.typeof data e') in
      spre, Some (UNARY (ADDROF, e', t)), spost
    | Cabs.PREINCR ->
      let* spre, e', spost, expanded = go_increment_operand u e in
      if expanded then return (spre, Some e', spost)
      else
        let n = CONST_INT (Word.one 8, UNSIGNED) in
        let* rhs = Helper.make_arith ADD e' n in
        let+ eff = Helper.make_assign e' rhs in
        sequence [spre; eff], Some e', spost
    | Cabs.POSINCR ->
      let* spre, e', spost, expanded = go_increment_operand u e in
      if expanded then return (spre, Some e', spost)
      else
        let n = CONST_INT (Word.one 8, UNSIGNED) in
        let* rhs = Helper.make_arith ADD e' n in
        let+ eff = Helper.make_assign e' rhs in
        spre, Some e', sequence [spost; eff]
    | Cabs.PREDECR ->
      let* spre, e', spost, expanded = go_increment_operand u e in
      if expanded then return (spre, Some e', spost)
      else
        let n = CONST_INT (Word.one 8, UNSIGNED) in
        let* rhs = Helper.make_arith SUB e' n in
        let+ eff = Helper.make_assign e' rhs in
        sequence [spre; eff], Some e', spost
    | Cabs.POSDECR ->
      let* spre, e', spost, expanded = go_increment_operand u e in
      if expanded then return (spre, Some e', spost)
      else
        let n = CONST_INT (Word.one 8, UNSIGNED) in
        let* rhs = Helper.make_arith SUB e' n in
        let+ eff = Helper.make_assign e' rhs in
        spre, Some e', sequence [spost; eff]

  (* Expand the increment into the operand if necessary. *)
  and go_increment_operand
      (u : Cabs.unary_operator)
      (e : Cabs.expression) : (stmt * exp * stmt * bool) transl =
    let lval = go_expression_lvalue "go_increment_operand" in
    if Cabs.is_lvalue e then match e with
      | Cabs.QUESTION (c, l, r) ->
        let+ pre, e, post =
          go_question c
            Cabs.(UNARY (u, l)) Cabs.(UNARY (u, r)) in
        let e = Option.value_exn e in
        pre, e, post, true
      | _ ->
        let+ pre, e, post = lval e in
        pre, e, post, false
    else
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      fail (
        sprintf "Patch_c.go_increment_operand: \
                 expression:\n\n%s\n\nis not an l-value" s)

  (* Translate binary operators. *)
  and go_binary_operator
      (b : Cabs.binary_operator)
      (lhs : Cabs.expression)
      (rhs : Cabs.expression) : eexp transl =
    let exp = go_expression_strict "go_binary_operator" in
    let default ?(no_ptr = false) op =
      let* spre1, e1, spost1 = exp lhs in
      let* {data; csize; _} = get () in
      let t1 = Exp.typeof data e1 in
      let* spre2, e2, spost2 = exp rhs in
      let t2 = Exp.typeof data e2 in
      let* t, e1, e2 =
        let default () = match Type.unify data csize t1 t2 with
          | None -> typ_unify_error Cabs.(BINARY (b, lhs, rhs)) t1 t2
          | Some t ->
            let e1 = Exp.with_type data csize e1 t in
            let e2 = Exp.with_type data csize e2 t in
            return (t, e1, e2) in
        match t1, t2 with
        | `Basic {C.Type.Spec.t = #C.Type.integer; _},
          `Basic {C.Type.Spec.t = #C.Type.integer; _} -> begin
            (* The size of a constant integer is ambiguous until
               we use it in some kind of operation. *)
            match e1, e2 with
            | CONST_INT _, _ ->
              return (t2, Exp.with_type data csize e1 t2, e2)
            | _, CONST_INT _ ->
              return (t1, e1, Exp.with_type data csize e2 t1)
            | _ -> default ()
          end
        | _ -> default () in
      match t with
      | `Void when no_ptr ->
        typ_error Cabs.(BINARY (b, lhs, rhs)) t
          "Expected integral type"
      | `Void ->
        typ_error Cabs.(BINARY (b, lhs, rhs)) t
          "Expected integral or pointer type"
      | (`Pointer _) as t when no_ptr ->
        typ_error Cabs.(BINARY (b, lhs, rhs)) t
          "Pointer type is not allowed"
      | _ ->
        let e1 = Exp.with_type data csize e1 t in
        let e2 = Exp.with_type data csize e2 t in
        let f op e1 e2 t = return @@ BINARY (op, e1, e2, t) in
        Helper.binary_tmp_or_simple op
          spre1 e1 spost1
          spre2 e2 spost2
          t ~f in
    match b with
    | Cabs.ADD -> go_arith ADD b lhs rhs
    | Cabs.SUB -> go_arith SUB b lhs rhs
    | Cabs.MUL -> go_arith MUL b lhs rhs ~no_ptr:true
    | Cabs.DIV -> go_arith DIV b lhs rhs ~no_ptr:true
    | Cabs.MOD -> go_arith MOD b lhs rhs ~no_ptr:true
    | Cabs.AND -> go_short_circuit_and lhs rhs
    | Cabs.OR -> go_short_circuit_or lhs rhs
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
    | Cabs.ASSIGN -> go_assign lhs rhs
    | Cabs.ADD_ASSIGN -> go_compound ADD b lhs rhs
    | Cabs.SUB_ASSIGN -> go_compound SUB b lhs rhs
    | Cabs.MUL_ASSIGN -> go_compound MUL b lhs rhs ~no_ptr:true
    | Cabs.DIV_ASSIGN -> go_compound DIV b lhs rhs ~no_ptr:true
    | Cabs.MOD_ASSIGN -> go_compound MOD b lhs rhs ~no_ptr:true
    | Cabs.BAND_ASSIGN -> go_compound LAND b lhs rhs ~no_ptr:true
    | Cabs.BOR_ASSIGN -> go_compound LOR b lhs rhs ~no_ptr:true
    | Cabs.XOR_ASSIGN -> go_compound XOR b lhs rhs ~no_ptr:true
    | Cabs.SHL_ASSIGN -> go_compound SHL b lhs rhs ~no_ptr:true
    | Cabs.SHR_ASSIGN -> go_compound SHR b lhs rhs ~no_ptr:true

  (* Captures the short-circuiting semantics of the && operator. *)
  and go_short_circuit_and
      (lhs : Cabs.expression)
      (rhs : Cabs.expression) : eexp transl =
    let exp = go_expression_strict "go_short_circuit_and" in
    let* width = gets @@ fun {target; _} ->
      Theory.Target.data_addr_size target in
    let* spre1, e1, spost1 = exp lhs in
    let* {data; csize; _} = get () in
    let t1 = Exp.typeof data e1 in
    let* spre2, e2, spost2 = exp rhs in
    let t2 = Exp.typeof data e2 in
    match Type.unify_ptr_to_int data csize width t1 t2 with
    | None -> typ_unify_error Cabs.(BINARY (AND, lhs, rhs)) t1 t2
    | Some t ->
      let e1 = Exp.with_type data csize e1 t in
      let e2 = Exp.with_type data csize e2 t in
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

  (* Captures the short-circuiting semantics of the || operator. *)
  and go_short_circuit_or
      (lhs : Cabs.expression)
      (rhs : Cabs.expression) : eexp transl =
    let exp = go_expression_strict "go_short_circuit_or" in
    let* width = gets @@ fun {target; _} ->
      Theory.Target.data_addr_size target in
    let* spre1, e1, spost1 = exp lhs in
    let* {data; csize; _} = get () in
    let t1 = Exp.typeof data e1 in
    let* spre2, e2, spost2 = exp rhs in
    let t2 = Exp.typeof data e2 in
    match Type.unify_ptr_to_int data csize width t1 t2 with
    | None -> typ_unify_error Cabs.(BINARY (OR, lhs, rhs)) t1 t2
    | Some t ->
      let e1 = Exp.with_type data csize e1 t in
      let e2 = Exp.with_type data csize e2 t in
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

  (* Compound binary operator (+=, -=, *=, etc). *)
  and go_compound
      ?(no_ptr : bool = false)
      (b : binop)
      (b' : Cabs.binary_operator)
      (lhs : Cabs.expression)
      (rhs : Cabs.expression) : eexp transl =
    let lval = go_expression_lvalue "go_compound" in
    let exp = go_expression_strict "go_compound" in
    match lhs with
    | Cabs.QUESTION (c, l, r) when Cabs.is_lvalue lhs ->
      let l = Cabs.(BINARY (b', l, rhs)) in
      let r = Cabs.(BINARY (b', r, rhs)) in
      go_question c l r
    | _ ->
      let* spre1, e1, spost1 = lval lhs in
      let* spre2, e2, spost2 = exp rhs in
      let e = Cabs.(BINARY (b', lhs, rhs)) in
      let* e' = Helper.make_arith b e1 e2 ~e ~no_ptr in
      go_assign_aux spre1 e1 spost1 spre2 e' spost2 ~e ~lhs

  (* Generic arithmetic operator. *)
  and go_arith
      ?(no_ptr : bool = false)
      (b : binop) (b' : Cabs.binary_operator)
      (lhs : Cabs.expression) (rhs : Cabs.expression) : eexp transl =
    let exp = go_expression_strict "go_arithmetic" in
    let e = Cabs.(BINARY (b', lhs, rhs)) in
    let* spre1, e1, spost1 = exp lhs in
    let* spre2, e2, spost2 = exp rhs in
    let f op e1 e2 _ = Helper.make_arith op e1 e2 ~e ~no_ptr in
    Helper.binary_tmp_or_simple b
      spre1 e1 spost1
      spre2 e2 spost2
      `Void ~f

  (* Compile an assignment expression. *)
  and go_assign
      ?(lhs_pre : exp option = None)
      (lhs : Cabs.expression) (rhs : Cabs.expression) : eexp transl =
    let exp ?(assign = None) =
      go_expression_strict "go_assign" ~assign in
    let lval = go_expression_lvalue "go_assign" in
    match lhs with
    | Cabs.QUESTION (c, l, r) when Cabs.is_lvalue lhs ->
      go_question c
        Cabs.(BINARY (ASSIGN, l, rhs)) Cabs.(BINARY (ASSIGN, r, rhs))
    | _ ->
      let* spre1, e1, spost1 = match lhs_pre with
        | Some e -> return (NOP, e, NOP)
        | None -> lval lhs in
      let* spre2, e2, spost2 = match e1 with
        | VARIABLE v -> exp rhs ~assign:(Some v)
        | _ -> exp rhs in
      go_assign_aux
        spre1 e1 spost1 spre2 e2 spost2 ~lhs
        ~e:Cabs.(BINARY (ASSIGN, lhs, rhs))

  (* The operands have been compiled and their side effects are explicit,
     so continue compiling the assignment. *)
  and go_assign_aux
      ?(lhs : Cabs.expression = NOTHING)
      ?(e : Cabs.expression = NOTHING)
      (spre1 : stmt) (e1 : exp) (spost1 : stmt)
      (spre2 : stmt) (e2 : exp) (spost2 : stmt) : eexp transl =
    let* is_store = match e1 with
      | VARIABLE (_, _) -> return false
      | UNARY (MEMOF, _, _) -> return true
      | _ ->
        let msg = Format.sprintf
            "Patch_c.go_assign: expected an l-value for LHS \
             of assignment, got:\n\n%s\n" @@
          Utils.print_c Cprint.print_statement Cabs.(COMPUTATION lhs) in
        fail msg in
    (* We follow order of evaluation as right-to-left. *)
    let* te2 = Helper.new_tmp_or_simple spre2 e2 spost2 in
    match te2 with
    | First e2 ->
      let+ s = Helper.make_assign e1 e2 ~is_store ~e in
      sequence [spre1; s], Some e1, spost1
    | Second (spre2, e2, spost2) ->
      let+ s = Helper.make_assign e1 e2 ~is_store ~e in
      sequence [spre2; spost2; spre1; s], Some e1, spost1

  (* Translate the ternary operator. *)
  and go_question
      ?(lval = false)
      ?(assign : var option = None)
      (cond : Cabs.expression)
      (then_ : Cabs.expression)
      (else_ : Cabs.expression) : eexp transl =
    let e = Cabs.(QUESTION (cond, then_, else_)) in
    let exp =
      (if lval
       then go_expression_lvalue
       else go_expression_strict)
        ~assign "go_question" in
    let* scondpre, cond, scondpost = exp cond in
    let* sthenpre, ethen, sthenpost = exp then_ in
    let* selsepre, eelse, selsepost = exp else_ in
    let* {data; csize; _} = get () in
    let t1 = Exp.typeof data ethen in
    let t2 = Exp.typeof data eelse in
    match Type.unify data csize t1 t2 with
    | None -> typ_unify_error e t1 t2
    | Some `Void ->
      let+ dummy = new_tmp `Void in
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
      let ethen = Exp.with_type data csize ethen t in
      let eelse = Exp.with_type data csize eelse t in
      let+ v = match assign with
        | Some v -> return v
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

  and go_call_args
      ?(e : Cabs.expression = NOTHING)
      (args : Cabs.expression list)
      (targs : typ list) : eexp_strict list transl =
    let exp = go_expression_strict "go_call_args" in
    match List.zip args targs with
    | Ok l ->
      (* Evaluated left to right. *)
      let* {data; csize; _} = get () in
      Transl.List.fold_right l ~init:[] ~f:(fun (arg, t) acc ->
          let* spre, a, spost = exp arg in
          let ta = Exp.typeof data a in
          match Type.cast_assign data csize t ta a with
          | Some (t, a) ->
            let a = Exp.with_type data csize a t in
            return ((spre, a, spost) :: acc)
          | None ->
            let s, a =
              Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e),
              Utils.print_c Cprint.print_statement Cabs.(COMPUTATION arg) in
            let msg = Format.asprintf
                "Patch_c.go_call_args:\n\n%s\n\n\
                 argument %s has type %a but type %a was \
                 expected" s a C.Type.pp ta C.Type.pp t in
            fail msg)
    | Unequal_lengths ->
      let msg = Format.sprintf
          "Patch_c.go_call_args: expected %d arguments, got %d:\n\n%s\n"
          (List.length targs) (List.length args) @@
        Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      fail msg

  and go_call
      ?(assign : var option = None)
      ?(computation : bool = false)
      (f : Cabs.expression)
      (args : Cabs.expression list) : (stmt * exp option) transl =
    let e = Cabs.CALL (f, args) in
    let exp = go_expression_strict "go_call" in
    let* sfpre, f', sfpost = exp f in
    let* {data; csize; _} = get () in
    match Exp.typeof data f' with
    | `Pointer {C.Type.Spec.t = `Function {C.Type.Spec.t = proto; _}; _}
    | `Function {C.Type.Spec.t = proto; _} ->
      let targs = List.map proto.args ~f:snd in
      let* args = go_call_args args targs ~e in
      let* eff, args' =
        (* Sequence the effects of each arg expression. *)
        let rec aux (pre, args, post) = function
          | [] -> return (sequence [pre; post], List.rev args)
          | (spre, e, spost) :: rest ->
            let* te = Helper.new_tmp_or_simple spre e spost in
            let eff, e, spost = match te with
              | First e -> sequence [pre; post], e, NOP
              | Second (spre, e, spost) ->
                sequence [pre; post; spre], e, spost in
            aux (eff, e :: args, spost) rest in
        aux (sfpre, [], sfpost) args in
      let is_void = match Type.unify data csize proto.return `Void with
        | Some `Void -> true
        | Some _ -> false
        | None -> false in
      if computation then
        return (sequence [eff; CALL (f', args')], None)
      else if is_void then
        let+ dummy = new_tmp `Void in
        sequence [eff; CALL (f', args')], Some (VARIABLE dummy)
      else
        (* Do we already know who we're assigning to? *)
        let+ v = match assign with
          | None -> new_tmp proto.return
          | Some (v, t) ->
            (* Type checking. Use a dummy RHS since calls are not
               expressions. *)
            let dummy = CONST_INT (Word.of_int ~width:8 42, UNSIGNED) in
            match Type.cast_assign data csize t proto.return dummy with
            | Some _ -> return (v, t)
            | None ->
              let s =
                Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
              let msg = Format.asprintf
                  "Patch_c.go_call:\n\n%s\n\nhas return type %a, cannot \
                   unify with var %a of type %a"
                  s C.Type.pp proto.return Theory.Var.pp v C.Type.pp t in
              fail msg in
        sequence [eff; CALLASSIGN (v, f', args')], Some (VARIABLE v)
    | t ->
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      let msg = Format.asprintf
          "Patch_c.go_call:\n\n%s\n\nhas type %a, expected function type"
          s C.Type.pp t in
      fail msg

  (* This function returns the side effects, the pointer to the element in the
     array (as an integer), and the element type.

     When indexing into a one-or-multidimensional array, the memory layout is
     assumed to be flat, whereas a pointer (or multi-pointer) is accessed
     through the layers of memory indirection. Currently, we only support the
     latter form.
  *)
  and go_index
      (ptr : Cabs.expression)
      (idx : Cabs.expression) : (eexp * typ) transl =
    let exp = go_expression_strict "go_index" in
    let* sptrpre, eptr, sptrpost = exp ptr in
    let* sidxpre, eidx, sidxpost = exp idx in
    let* {target; data; csize; _} = get () in
    let width = Theory.Target.data_addr_size target in
    let tptr = Exp.typeof data eptr in
    let tidx = Exp.typeof data eidx in
    match tidx with
    | `Basic {C.Type.Spec.t = #C.Type.integer; _} -> begin
        let make_ptr scale () eptr eidx tidx =
          BINARY (
            ADD,
            CAST (tidx, eptr),
            BINARY (
              MUL,
              CONST_INT (scale, UNSIGNED),
              eidx,
              tidx),
            tidx) |>
          return in
        match tptr with
        | `Pointer {C.Type.Spec.t; _} ->
          let size = Option.value_exn (csize#bits t) in
          let scale = Word.of_int ~width (size lsr 3) in
          let tidx = int_typ data (Size.of_int_exn width) UNSIGNED in
          let eidx = Exp.with_type data csize eidx tidx in
          let+ e = Helper.binary_tmp_or_simple ()
              sptrpre eptr sptrpost
              sidxpre eidx sidxpost
              tidx ~f:(make_ptr scale) in
          e, t
        | _ -> 
          let e = Cabs.(INDEX (ptr, idx)) in
          let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
          let msg = Format.asprintf
              "Patch_c.go_index: in expression:\n\n%s\n\nArray operand \
               has type %a. Expected pointer.\n" s C.Type.pp tptr in
          fail msg
      end
    | _ ->
      let e = Cabs.(INDEX (ptr, idx)) in
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      let msg = Format.asprintf
          "Patch_c.go_index: in expression:\n\n%s\n\nIndex operand \
           has type %a. Expected integer.\n" s C.Type.pp tidx in
      fail msg

  and go_memberof (ptr : Cabs.expression) (field : string) : eexp transl =
    let exp = go_expression_strict "go_memberof" in
    let* sptrpre, eptr, sptrpost = exp ptr in
    let* {data; csize; _} = get () in
    let tptr = Exp.typeof data eptr in
    match tptr with
    | `Pointer {C.Type.Spec.t; _} -> begin
        match t with
        | `Structure {C.Type.Spec.t = compound; _} -> begin
            match Type.field_offset csize field t with
            | Some off ->
              let _, tfield =
                List.find_exn compound.fields ~f:(fun (name, _) ->
                    String.equal name field) in
              let tfield' = C.Type.pointer tfield in
              let tarith = int_typ data (csize#pointer :> size) UNSIGNED in
              let eptr = Exp.with_type data csize eptr tarith in
              let e =
                UNARY (
                  MEMOF,
                  CAST (
                    tfield',
                    BINARY (
                      ADD,
                      eptr,
                      CONST_INT (off, UNSIGNED),
                      tarith)),
                  tfield) in
              return (sptrpre, Some e, sptrpost)
            | None ->
              let e = Cabs.(MEMBEROFPTR (ptr, field)) in
              let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
              let msg = Format.sprintf
                  "Patch_c.go_memberof: in expression:\n\n%s\n\nstruct %s \
                   has no such field %s\n" s compound.name field in
              fail msg
          end
        | _ ->
          let e = Cabs.(MEMBEROFPTR (ptr, field)) in
          let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
          let msg = Format.asprintf
              "Patch_c.go_memberof: in expression:\n\n%s\n\nexpected \
               struct, got %a\n" s C.Type.pp tptr in
          fail msg
      end
    | _ ->
      let e = Cabs.(MEMBEROFPTR (ptr, field)) in
      let s = Utils.print_c Cprint.print_statement Cabs.(COMPUTATION e) in
      let msg = Format.asprintf
          "Patch_c.go_memberof: in expression:\n\n%s\n\nexpected \
           pointer, got %a\n" s C.Type.pp tptr in
      fail msg

  (* Translate a statement. *)
  and go_statement (s : Cabs.statement) : stmt transl = match s with
    | Cabs.NOP -> return NOP
    | Cabs.COMPUTATION e -> begin
        let* spre, e, spost = go_expression e ~computation:true in
        match e with
        | None -> return @@ sequence [spre; spost]
        | Some e ->
          let* {data; _} = get () in
          let+ v = new_tmp @@ Exp.typeof data e in
          sequence [spre; ASSIGN (v, e); spost]
      end
    | Cabs.BLOCK body ->
      let+ body = go_body body in
      BLOCK body
    | Cabs.SEQUENCE (s1, s2) ->
      let* s1 = go_statement s1 in
      let+ s2 = go_statement s2 in
      SEQUENCE (s1, s2)
    | Cabs.IF (cond, then_, else_) ->
      let* scondpre, cond, scondpost =
        go_expression_strict "go_statement (IF)" cond in
      let* then_ = go_statement then_ in
      let+ else_ = go_statement else_ in
      sequence [
        scondpre;
        IF (
          cond,
          sequence [scondpost; then_],
          sequence [scondpost; else_]);
      ]
    | Cabs.LABEL (l, s) ->
      let* {tenv; _} = get () in
      let+ stmt = go_statement s in
      BLOCK {tenv; stmt; label = Some l}
    | Cabs.GOTO lbl -> return @@ GOTO lbl
    | _ ->
      let msg = Format.sprintf
          "Patch_c.go_statement: unsupported:\n\n%s\n" @@
        Utils.print_c Cprint.print_statement s in
      fail msg

end

let fail (msg : string) : 'a KB.t = KB.fail @@ Errors.Patch_c msg

(* Get the C data model of the target, if we support it. *)
let data_of_tgt (target : Theory.target) : Data_model.t KB.t =
  let fail () =
    fail @@ Format.asprintf "Unsupported target %a"
      Theory.Target.pp target in
  if CT.is_arm32 target || CT.is_ppc32 target then
    KB.return Data_model.{sizes = `ILP32; schar = false}
  else fail ()

(* Translate a definition. *)
let translate (patch : Cabs.definition) ~(target : Theory.target) : t KB.t =
  let open KB.Let in
  let* data = data_of_tgt target in
  let csize = new C.Size.base data.sizes in
  let* body = match patch with
    | FUNDEF (_, b) -> KB.return b
    | _ ->
      let s = Utils.print_c Cprint.print_def patch in
      fail @@ sprintf "Patch_c.translate: unexpected patch shape:\n\n%s\n\n\
                       expected a single function definition" s in
  (* Perform type-checking and elaboration. *)
  let* body, _ =
    let open Transl in
    Env.create ~target ~data ~csize () |>
    run (Main.go_body body) in
  (* Perform some simplification passes. *)
  let s = Opt.Cast.go data body.stmt in
  let s = Opt.Unused.remove s in
  let s = Opt.Nops.go s in
  (* Success! *)
  let prog = {data; csize; body = {body with stmt = s}} in
  Log.send "Translated to the following PatchC program:\n%s" @@ to_string prog;
  KB.return prog
