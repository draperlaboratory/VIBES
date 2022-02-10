open Bap.Std
open Bap_core_theory

type cls
type t = cls KB.obj
type computed = (cls, unit) KB.cls KB.value

let package = "vibes"
let name = "congruent-temps"
let cls : (cls, unit) KB.cls = KB.Class.declare ~package name ()

let slot : (cls, (var * var) option) KB.slot =
  KB.Class.property cls ~package "congruent-vars" @@
  KB.Domain.optional "congruent-vars-domain"
    ~equal:(fun (a1, b1) (a2, b2) -> Var.(a1 = a2) && Var.(b1 = b2))
