open Core
open Bap.Std

module Attr = Vibes_constants.Attr

let spill : unit tag = Value.Tag.register (module Unit)
    ~name:(Attr.make "spilled")
    ~uuid:"a770b736-07d1-11ed-84a7-7f2a318d4806"

let argument : unit tag = Value.Tag.register (module Unit)
    ~name:(Attr.make "argument")
    ~uuid:"2ecbeae4-a576-406c-b95c-324083406c85"

module Var_set = struct

  include Var.Set

  let pp (ppf : Format.formatter) (s : t) : unit =
    Format.fprintf ppf "%s" @@
    List.to_string ~f:Var.to_string @@
    Set.to_list s

end

module Var_map = struct

  type t = Var.Set.t Var.Map.t
  [@@deriving bin_io, sexp, compare]

  let pp (ppf : Format.formatter) (m : t) : unit =
    Format.fprintf ppf "%s" @@
    List.to_string ~f:(fun (x, y) ->
        Format.sprintf "%s %s" (Var.to_string x)
          (List.to_string ~f:Var.to_string @@ Set.to_list y)) @@
    Map.to_alist m

end

let ins : Var.Set.t tag = Value.Tag.register (module Var_set)
    ~name:(Attr.make "ins")
    ~uuid:"f5274885-8155-427c-8d9f-7efebaf00827"

let outs : Var.Set.t tag = Value.Tag.register (module Var_set)
    ~name:(Attr.make "outs")
    ~uuid:"dd9a51b0-bfed-41e3-82a7-eba42a242428"

let congruences : Var.Set.t Var.Map.t tag =
  Value.Tag.register (module Var_map)
    ~name:(Attr.make "congruences")
    ~uuid:"62fc18a7-61ff-4ee3-b02d-9e4a6b18cea9"
