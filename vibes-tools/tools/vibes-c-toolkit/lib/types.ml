open Core
open Bap.Std

module T = Bap_core_theory.Theory

type Vibes_error_lib.Std.t +=
  | Higher_var_not_substituted of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | Higher_var_not_substituted s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer

module Func_info = struct

  type t = {
    label : T.Label.t;
    args : var list;
  }

  let create label args : t = { label; args }

  let to_string t : string =
    let label_name = Tid.name t.label in
    let arg_names = List.map t.args ~f:Var.name in
    let arg_names_to_print = String.concat arg_names ~sep:", " in
    Format.sprintf "%s: %s" label_name arg_names_to_print

  let to_sexp t : Sexp.t =
    let label_name = Tid.name t.label in
    let arg_names = List.map t.args ~f:Var.name in
    let args = List.map arg_names ~f:(fun a -> Sexp.Atom a) in
    let header = Sexp.List [Sexp.Atom "label"; Sexp.Atom label_name] in
    let body = Sexp.List [Sexp.Atom "args"; Sexp.List args] in
    Sexp.List [header; body]

  let equal t1 t2 : bool =
    Tid.equal t1.label t2.label &&
    (List.equal Var.equal t1.args t2.args)

  let rec find (tid : Tid.t) (ts : t list) : var list option =
    match ts with
    | [] -> None
    | hd :: tl ->
       if Tid.equal tid hd.label then Some hd.args
       else find tid tl

end
