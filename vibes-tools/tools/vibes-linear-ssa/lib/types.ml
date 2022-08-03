open Core
open Bap.Std

module Naming = Var_higher_vars_lib.Substituter.Naming

(*
type Vibes_error_lib.Std.t +=
  | Other of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | Other s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer
*)

let prefix_of_tid (tid : tid) : string =
  let tid_str = Tid.to_string tid in
  String.drop_prefix tid_str 1

(* Use the tid of the blk as the prefix, dropping the '%' at
   the beginning. *)
let prefix_from (blk : blk term) : string =
  prefix_of_tid @@ Term.tid blk

let linearize ~(prefix : string) (var : var) : var =
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
  let (let*) x f = Option.bind x ~f in
  let name = String.drop_prefix name prefix_len in
  let name, is_reg =
    Naming.unmark_reg_name name |>
    Option.value_map ~default:(name, false) ~f:(fun name ->
        name, true) in
  let* name = match String.split name ~on:'_' with
    | [] -> Some name
    | [name] when not @@ String.is_empty name -> Some name
    | l ->
      let* l = List.drop_last l in
      Some (String.concat l ~sep:"_")
  in
  if is_reg then Some (Naming.mark_reg_name name) else Some name

let same (a : var) (b : var) : bool =
  let a = Var.name a and b = Var.name b in
  String.equal a b ||
  match (orig_name a), (orig_name b) with
  | Some a, Some b -> String.equal a b
  | _ ->  false

let congruent (a : var) (b : var) : bool =
  let name_1 = String.drop_prefix (Var.name a) prefix_len in
  let name_2 = String.drop_prefix (Var.name b) prefix_len in
  (* We have to be careful feeding in vars which don't fit our naming
     convention for linear SSA. In particular, the instruction selector
     may generate additional temporary variables, which happens after
     we've run the linear SSA pass. *)
  if String.is_empty name_1 && String.is_empty name_2 then false
  else String.equal name_1 name_2

module Linear = struct

  module Env = struct

    type t = {
      vars : Var.Set.t;
      prefix : string;
    }

    let empty = {
      vars = Var.Set.empty;
      prefix = "";
    }

    let add_var v env = {env with vars = Set.add env.vars v}
    let with_prefix prefix env = {env with prefix}

  end

  include Monad.State.T1(Env)(Monad.Ident)
  include Monad.State.Make(Env)(Monad.Ident)

end
