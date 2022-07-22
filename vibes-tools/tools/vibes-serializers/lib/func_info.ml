open Core
open Bap.Std
open Vibes_error_lib.Let

module T = Bap_core_theory.Theory
module Err = Vibes_error_lib.Std
module Func_info = Vibes_c_toolkit_lib.Types.Func_info

module Deserializer = struct

  let s_of = Sexp.to_string

  let word_size (target : T.Target.t) : typ =
    let bits = T.Target.bits target in
    Imm bits

  let deserialize_label (sexp : Sexp.t) : (Tid.t, Err.t) result =
    match sexp with
    | Sexp.List [Atom "label"; Atom raw_label] ->
       begin
         match Tid.from_string raw_label with
         | Ok tid -> Ok tid
         | Error e ->
             let err = Error.to_string_hum e in
             let msg = Format.sprintf "Invalid label: '%s'" err in
             Error (Types.Invalid_func_info msg)
       end
    | _ ->
       let msg =
         Format.sprintf "Expected '(label x)', but got: '%s'" (s_of sexp)
       in
       Error (Types.Invalid_func_info msg)

  let deserialize_arg ~(target : T.Target.t) (sexp : Sexp.t) : (Var.t, Err.t) result =
    match sexp with
    | Sexp.Atom name ->
      let size = word_size target in
      Ok (Var.create name size)
    | _ ->
       let msg = Format.sprintf
         "Expected variable name (a Sexp.Atom), but got '%s'" (s_of sexp)
       in
       Error (Types.Invalid_func_info msg)

  let of_sexp ~(target : T.Target.t) (sexp : Sexp.t)
      : (Func_info.t, Err.t) result =
    match sexp with
    | Sexp.List [
        raw_label;
        Sexp.List [Sexp.Atom "args"; Sexp.List raw_args]
      ] ->
        let- label = deserialize_label raw_label in
        let- args = Result.all
          (List.map raw_args ~f:(fun arg -> deserialize_arg arg ~target))
        in
        Ok (Func_info.create label args) 
    | _ ->
       let msg = Format.sprintf 
         "Expected '((label a) (args (x y z)))' but got: '%s'"
         (s_of sexp)
       in
       Error (Types.Invalid_func_info msg)

end

let serialize func_info = Ok (Func_info.to_sexp func_info)
let deserialize = Deserializer.of_sexp
