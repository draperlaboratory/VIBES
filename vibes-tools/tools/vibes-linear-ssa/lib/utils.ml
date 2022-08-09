open Core
open Bap.Std
open Monads.Std

module Naming = Vibes_higher_vars.Substituter.Naming

type prefix = string

let prefix_of_tid (tid : tid) : prefix =
  let tid_str = Tid.to_string tid in
  String.drop_prefix tid_str 1

let linearize ~(prefix : prefix) (var : var) : var =
  let name = Var.name var in
  let typ = Var.typ var in
  let new_name = prefix ^ "_" ^ name in
  let escaped_name =
    String.substr_replace_all new_name ~pattern:"." ~with_:"_" in
  Var.create escaped_name typ

(* The prefix consists of an underscore (1 char), followed by the
   tid string (8 chars), ending with another underscore (1 char). *)
let prefix_len = 10

let orig_name (name : string) : string option =
  let open Monad.Option.Let in
  let name = String.drop_prefix name prefix_len in
  let name, is_reg =
    Naming.unmark_reg_name name |>
    Option.value_map ~default:(name, false)
      ~f:(fun name -> name, true) in
  let+ name = match String.split name ~on:'_' with
    | [] -> Some name
    | [""] -> None
    | [name] -> Some name
    | split ->
      let+ split = List.drop_last split in
      String.concat split ~sep:"_" in
  if is_reg then Naming.mark_reg_name_unsafe name else name

let same (x : var) (y : var) : bool =
  let x = Var.name x in
  let y = Var.name y in
  String.equal x y || match orig_name x, orig_name y with
  | Some x, Some y -> String.(x = y)
  | _ ->  false

let congruent (x : var) (y : var) : bool =
  let x = String.drop_prefix (Var.name x) prefix_len in
  let y = String.drop_prefix (Var.name y) prefix_len in
  match x, y with
  | "", _ | _, "" -> false
  | _ -> String.(x = y)
