open Bap_core_theory

module T = Theory

open KB.Syntax

module C_toolkit = Vibes_c_toolkit
module Hvar = Vibes_higher_vars.Higher_var
module Constants = Vibes_constants

module C_semantics = struct

  let package = Constants.Bap_kb.package

  let slot : (T.program, T.Semantics.t) KB.slot =
    KB.Class.property T.Program.cls "c-semantics"
      T.Semantics.domain ~package

  (* Our provided semantics for the C code needs to be available
     to the rest of BAP, which we do by promising to compute the
     [Theory.Semantics.slot]. This is the property that BAP uses
     as the actual semantics of the program. *)
  let () = KB.promise T.Semantics.slot @@ KB.collect slot

end

let to_core
    (ast : Types.ast)
    (target : T.target)
    (hvars : Hvar.t list) : T.label KB.t =
  let* label = T.Label.fresh in
  let* unit = KB.Object.create T.Unit.cls in
  let* () = KB.provide T.Unit.target unit target in
  let* () = KB.provide T.Label.unit label @@ Some unit in
  let* theory = T.instance () in
  let* (module Core) = T.require theory in
  let module C_compiler = C_toolkit.Core_c.Make(Core) in
  let* sem = C_compiler.compile hvars target ast in
  let+ () = KB.provide C_semantics.slot label sem in
  label
