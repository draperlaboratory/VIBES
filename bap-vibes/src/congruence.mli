(** The Knowledge Base class for relating congruence of temporary vars that
    are generated during the compiler pipeline. *)

open Bap.Std
open Bap_core_theory

type cls
type t = cls KB.obj
type computed = (cls, unit) KB.cls KB.value

val name : string
val cls : (cls, unit) KB.cls

(** Our domain simply relates two vars. This relation is symmetric. *)
val slot : (cls, (var * var) option) KB.slot
