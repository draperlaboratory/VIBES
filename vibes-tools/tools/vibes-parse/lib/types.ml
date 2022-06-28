module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module Log = Vibes_log_lib.Stream
module Constants = Vibes_constants_lib

type ast = Cabs.definition

type Vibes_error_lib.Std.t +=
  | No_patch_code of string
  | Invalid_C of string
  | Invalid_sexp of string
  | Unknown_target of string
  | KB_error of string

let printer (e : Vibes_error_lib.Std.t) : string option =
  match e with
  | No_patch_code s -> Some s
  | Invalid_C s -> Some s
  | Invalid_sexp s -> Some s
  | Unknown_target s -> Some s
  | KB_error s -> Some s
  | _ -> None

let () = Vibes_error_lib.Std.register_printer printer

module Parsed_C_code = struct

  let package = Constants.Bap_kb.package

  let slot : (T.program, T.Semantics.t) KB.slot =
    KB.Class.property T.Program.cls "semantics" T.Semantics.domain ~package

  let set (label : T.Label.t) (semantics : T.Semantics.t) : unit KB.t =
    KB.provide slot label semantics

end

let () =
  KB.promise T.Semantics.slot
  @@ fun label -> KB.collect Parsed_C_code.slot label
