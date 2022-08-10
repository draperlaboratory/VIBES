open Bap_core_theory

module T = Theory

open KB.Syntax

module C_toolkit = Vibes_c_toolkit
module Hvar = Vibes_higher_vars.Higher_var
module Parsed_c_code = Types.Parsed_c_code

let create_compilation_unit (target : T.target) : T.Unit.t KB.t =
  let* compilation_unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target compilation_unit target in
  KB.return compilation_unit

let to_core
    (ast : Types.ast)
    (target : T.target)
    (hvars : Hvar.t list) : T.label KB.t =
  let* label = T.Label.fresh in
  let* compilation_unit = create_compilation_unit target in
  let* () = KB.provide T.Label.unit label @@ Some compilation_unit in
  let* theory = T.instance () in
  let* (module Core) = T.require theory in
  let module C_compiler = C_toolkit.Core_c.Make(Core) in
  let* sem, func_info = C_compiler.compile hvars target ast in
  let* () = Parsed_c_code.set label sem in
  let+ () = Parsed_c_code.stash_function_info label func_info in
  label
