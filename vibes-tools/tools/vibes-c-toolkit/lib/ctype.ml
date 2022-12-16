(* This is adapted from BAP's FrontC parser plugin. *)

open Core
open Bap_c.Std

let int (size : Cabs.size) (sign : Cabs.sign) : C.Type.basic = 
  match size, sign with
  | NO_SIZE,   (NO_SIGN | SIGNED) -> `sint
  | SHORT,     (NO_SIGN | SIGNED) -> `sshort
  | LONG,      (NO_SIGN | SIGNED) -> `slong
  | LONG_LONG, (NO_SIGN | SIGNED) -> `slong_long
  | NO_SIZE,   UNSIGNED           -> `uint
  | SHORT,     UNSIGNED           -> `ushort
  | LONG,      UNSIGNED           -> `ulong
  | LONG_LONG, UNSIGNED           -> `ulong_long

let char : Cabs.sign -> C.Type.basic = function
  | NO_SIGN  -> `char
  | SIGNED   -> `schar
  | UNSIGNED -> `uchar

let cv : unit C.Type.qualifier = {
  const    = false;
  volatile = false;
  restrict = ();
}

let cvr : bool C.Type.qualifier = {
  cv with restrict = false;
}

let restricted (cvr : _ C.Type.qualifier) : bool C.Type.qualifier = {
  cvr with restrict = true;
}

let spec (qualifier : 'a) (t : 'b) : ('a, 'b) C.Type.spec = {
  t;
  attrs = [];
  qualifier;
}

let restrict (t : C.Type.t) : C.Type.t = `Pointer (spec (restricted cvr) t)

let size : Cabs.expression -> int option = function
  | CONSTANT CONST_INT s -> Some (Int.of_string s)
  | _ -> None

let enum
    (_name : string)
    (fields : (string * int64 option) list) : C.Type.t =
  C.Type.basic (`enum fields)

let name_groups : Cabs.name_group list -> 'a list =
  List.concat_map ~f:(fun (_, _, ns) ->
      List.map ns ~f:(fun (n, t, attrs, _) -> n, t, attrs))

let single_names : Cabs.single_name list -> 'a list =
  List.map ~f:(fun (_, _, (n, t, attrs, _)) -> n, t, attrs)

let rec gnu_attr : Cabs.gnu_attr -> C.Type.attr option = function
  | GNU_NONE -> None
  | GNU_CALL (name, args) ->
    Some C.Type.Attr.{name; args = gnu_attrs_args args}
  | GNU_ID s -> Some C.Type.Attr.{name = s; args = []}
  | GNU_CST _ | GNU_EXTENSION | GNU_INLINE | GNU_TYPE_ARG _ -> None

and gnu_attrs_args : Cabs.gnu_attr list -> string list =
  List.filter_map ~f:(fun (a : Cabs.gnu_attr) -> match a with
      | GNU_ID s
      | GNU_CST
          (CONST_INT s | CONST_FLOAT s | CONST_CHAR s | CONST_STRING s) ->
        Some s
      | _ -> None)

let gnu_attrs : Cabs.gnu_attr list -> C.Type.attr list =
  List.filter_map ~f:gnu_attr

let with_attrs (attrs : C.Type.attr list) : C.Type.t -> C.Type.t =
  let add t = C.Type.Spec.{t with attrs = t.attrs @ attrs} in
  function
  | `Void        -> `Void
  | `Basic t     -> `Basic (add t)
  | `Pointer t   -> `Pointer (add t)
  | `Array t     -> `Array (add t)
  | `Structure t -> `Structure (add t)
  | `Union t     -> `Union (add t)
  | `Function t  -> `Function (add t)

type ('a, 'b) qs = ('a C.Type.qualifier, 'b) C.Type.spec

type qualifier = {
  apply : 'a 'b. ('a, 'b) qs -> ('a, 'b) qs;
}

let const : qualifier = {
  apply = fun t -> C.Type.Spec.{
      t with qualifier = C.Type.Qualifier.{
      t.qualifier with const = true}
    }
}

let volatile : qualifier = {
  apply = fun t -> C.Type.Spec.{
      t with qualifier = C.Type.Qualifier.{
      t.qualifier with volatile = true}
    }
}

let rec qualify (f : qualifier) : C.Type.t -> C.Type.t = function
  | `Basic t -> `Basic (f.apply t)
  | `Pointer t -> `Pointer (f.apply t)
  | `Structure s -> `Structure C.Type.Spec.{
      s with
      t = C.Type.Compound.{
          s.t with
          fields = List.map s.t.fields ~f:(fun (n, t) -> n, qualify f t)
        }
    }
  | x -> x

type gamma = string -> C.Type.t

type tag = {
  lookup : 'a. (string -> 'a list -> C.Type.t) -> string -> C.Type.t;
}

let ctype (gamma : gamma) (tag : tag) (t : Cabs.base_type) : C.Type.t =
  let rec ctype : Cabs.base_type -> C.Type.t = function
    | NO_TYPE | TYPE_LINE _ | OLD_PROTO _ | BITFIELD _
    | BUILTIN_TYPE _ | VOID -> `Void
    | BOOL -> C.Type.basic `bool
    | CHAR sign -> C.Type.basic @@ char sign
    | INT (size, sign) -> C.Type.basic @@ int size sign
    | FLOAT _ -> C.Type.basic `float
    | DOUBLE long -> C.Type.basic @@ if long then `long_double else `double
    | COMPLEX_FLOAT -> C.Type.basic `cfloat
    | COMPLEX_DOUBLE -> C.Type.basic `cdouble
    | COMPLEX_LONG_DOUBLE -> C.Type.basic `clong_double
    | PTR t -> C.Type.pointer @@ ctype t
    | RESTRICT_PTR t -> restrict @@ ctype t
    | ARRAY (et, ice) -> C.Type.array ?size:(size ice) @@ ctype et
    | STRUCT (n, []) -> tag.lookup C.Type.structure n
    | UNION (n, []) -> tag.lookup C.Type.union n
    | ENUM (n, []) -> tag.lookup enum n
    | STRUCT (n, fs) -> C.Type.structure n @@ fields @@ name_groups fs
    | UNION (n, fs) -> C.Type.union n @@ fields @@ name_groups fs
    | PROTO (r, args, variadic) ->
      C.Type.function_ ~variadic ~return:(ctype r) @@
      fields @@ single_names args
    | NAMED_TYPE name -> gamma name
    | ENUM (name, fs) -> enum name @@ enum_items name fs
    | CONST t -> qualify const @@ ctype t
    | VOLATILE t -> qualify volatile @@ ctype t
    | GNU_TYPE (a, t) -> with_attrs (gnu_attrs a) @@ ctype t
  and enum_items _ : Cabs.enum_item list -> (string * int64 option) list =
    List.map ~f:(fun (name, exp) -> match (exp : Cabs.expression) with
        | CONSTANT (CONST_INT x) ->
          name, Option.try_with @@ fun () -> Int64.of_string x
        | _ -> name, None)
  and fields xs = List.map xs ~f:(fun (name, t, a) ->
      name, with_attrs (gnu_attrs a) @@ ctype t) in
  ctype t

module To_cabs = struct

  let rec go : C.Type.t -> Cabs.base_type = function
    | `Void -> VOID
    | `Basic {C.Type.Spec.qualifier; t; _} ->
      apply_cv_qualifier (basic t) qualifier
    | `Pointer {C.Type.Spec.qualifier; t; _} ->
      apply_cvr_qualifier (go t) qualifier
    | `Array {C.Type.Spec.qualifier; t; _} ->
      apply_cvr_qualifier (array t) qualifier
    | `Structure {C.Type.Spec.t; _} -> structure t
    | `Union {C.Type.Spec.t; _} -> union t
    | `Function {C.Type.Spec.t; _} -> func t

  and apply_cv_qualifier
      (t : Cabs.base_type)
      (q : C.Type.cv C.Type.qualifier) : Cabs.base_type =
    let t = if q.const then Cabs.CONST t else t in
    let t = if q.volatile then Cabs.VOLATILE t else t in
    t

  and apply_cvr_qualifier
      (t : Cabs.base_type)
      (q : C.Type.cvr C.Type.qualifier) : Cabs.base_type =
    let t = if q.const then Cabs.CONST t else t in
    let t = if q.volatile then Cabs.VOLATILE t else t in
    let t = if q.restrict then Cabs.RESTRICT_PTR t else t in
    t

  and basic : C.Type.basic -> Cabs.base_type = function
    | `bool         -> BOOL
    | `char         -> CHAR NO_SIGN
    | `schar        -> CHAR SIGNED
    | `uchar        -> CHAR UNSIGNED
    | `sshort       -> INT (SHORT, SIGNED)
    | `ushort       -> INT (SHORT, UNSIGNED)
    | `sint         -> INT (NO_SIZE, SIGNED)
    | `uint         -> INT (NO_SIZE, UNSIGNED)
    | `slong        -> INT (LONG, SIGNED)
    | `ulong        -> INT (LONG, UNSIGNED)
    | `slong_long   -> INT (LONG_LONG, SIGNED)
    | `ulong_long   -> INT (LONG_LONG, UNSIGNED)
    | `float        -> FLOAT false
    | `double       -> DOUBLE false
    | `long_double  -> DOUBLE true
    | `cfloat       -> COMPLEX_FLOAT
    | `cdouble      -> COMPLEX_DOUBLE
    | `clong_double -> COMPLEX_LONG_DOUBLE
    | `enum items   ->
      let items = List.map items ~f:(function
          | name, None -> name, Cabs.NOTHING
          | name, Some value ->
            name, CONSTANT (CONST_INT (Int64.to_string value))) in
      ENUM ("", items)

  and array (a : C.Type.array) : Cabs.base_type =
    let e = go a.element in
    let s = match a.size with
      | None -> Cabs.NOTHING
      | Some s -> CONSTANT (CONST_INT (Int.to_string s)) in
    ARRAY (e, s)

  and structure (c : C.Type.compound) : Cabs.base_type =
    STRUCT (c.name, List.map c.fields ~f:field)

  and union (c : C.Type.compound) : Cabs.base_type =
    UNION (c.name, List.map c.fields ~f:field)

  and field ((name, t) : string * C.Type.t) : Cabs.name_group =
    let t = go t in
    t, NO_STORAGE, [name, t, [], NOTHING]

  and func (p : C.Type.proto) : Cabs.base_type =
    let r = go p.return in
    let args = List.map p.args ~f:(fun (name, t) ->
        let t = go t in
        t, Cabs.NO_STORAGE, (name, t, [], Cabs.NOTHING)) in
    PROTO (r, args, p.variadic)

end
