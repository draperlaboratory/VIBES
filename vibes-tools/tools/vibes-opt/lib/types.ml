open Core
open Bap.Std

type t = {
  sub : sub term;
  argument_tids : Tid.Set.t;
} [@@deriving fields]

let create = Fields.create
