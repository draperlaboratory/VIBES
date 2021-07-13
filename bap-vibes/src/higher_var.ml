(** Implements {!Higher_var}. *)

open !Core_kernel

type register = string
type framepointer = register
type offset = Bitvec.t

type stored_in =
  | Register of register
  | Memory of framepointer * offset

type t = {
  name : string;
  at_entry : stored_in;
  at_exit : stored_in;
}

let name t = t.name
let at_entry t = t.at_entry
let at_exit t = t.at_exit

let create (name : string) (at_entry : stored_in) (at_exit : stored_in) : t =
  { name; at_entry; at_exit; }

let equal_stored_in (v1 : stored_in) (v2 : stored_in) : bool =
  match (v1, v2) with
  | (Register reg1, Register reg2) -> String.equal reg1 reg2
  | (Memory (fp1, offset1), Memory (fp2, offset2)) ->
    (String.equal fp1 fp2) && (Bitvec.equal offset1 offset2)
  | _ -> false 

let equal (t1 : t) (t2 : t) : bool =
  (String.equal t1.name t2.name) &&
  (equal_stored_in t1.at_entry t2.at_entry) &&
  (equal_stored_in t1.at_exit t2.at_exit)

let sexp_of_loc (v : stored_in) : Sexp.t =
  match v with
  | Register reg -> Sexp.Atom reg
  | Memory (fp, bitv) ->
    let offset = Bitvec.to_string bitv in
    Sexp.List [Sexp.Atom "-"; Sexp.Atom fp; Sexp.Atom offset]

let sexp_of (v : stored_in) : Sexp.t =
  match v with
  | Register reg -> Sexp.Atom reg
  | Memory (fp, bitv) ->
    let offset = Bitvec.to_string bitv in
    let loc = Sexp.List [Sexp.Atom "-"; Sexp.Atom fp; Sexp.Atom offset] in
    Sexp.List [Sexp.Atom "load"; loc]

let is_reg (v : stored_in) : bool =
  match v with
  | Register _ -> true
  | _ -> false

let find (name : register) (vars : t list) : t option =
  List.find vars ~f:(fun t -> String.equal name t.name)
