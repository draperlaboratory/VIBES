open Core

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module Log = Vibes_log_lib.Stream
module Constants = Vibes_constants_lib
module Func_info = Vibes_c_toolkit_lib.Types.Func_info

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

type ast = Cabs.definition

module Parsed_c_code = struct

  let package = Constants.Bap_kb.package

  let func_infos_domain : Func_info.t list KB.Domain.t =
    KB.Domain.flat "func-infos-domain"
      ~equal:(List.equal Func_info.equal)
      ~inspect:(fun infos -> Sexp.List (List.map infos ~f:Func_info.to_sexp))
      ~empty:[]

  let func_infos_slot : (T.program, Func_info.t list) KB.slot =
    KB.Class.property T.Program.cls "func-infos" func_infos_domain ~package

  let stash_func_infos (label : T.Label.t)
      (func_infos : Func_info.t list) : unit KB.t =
    KB.provide func_infos_slot label func_infos

  let get_func_infos (label : T.Label.t) : Func_info.t list KB.t =
    KB.collect func_infos_slot label

  let slot : (T.program, T.Semantics.t) KB.slot =
    KB.Class.property T.Program.cls "semantics" T.Semantics.domain ~package

  let set (label : T.Label.t) (semantics : T.Semantics.t) : unit KB.t =
    KB.provide slot label semantics

end

let () =
  KB.promise T.Semantics.slot
  @@ fun label -> KB.collect Parsed_c_code.slot label
