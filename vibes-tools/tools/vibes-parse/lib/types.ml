open Core
open Bap_core_theory

module T = Theory
module Log = Vibes_log_lib.Stream
module Constants = Vibes_constants_lib
module Function_info = Vibes_function_info_lib.Types

type ast = Cabs.definition

module Parsed_c_code = struct

  let package = Constants.Bap_kb.package

  let function_info_domain : Function_info.t KB.Domain.t =
    KB.Domain.flat "function-info-domain"
      ~equal:Function_info.equal
      ~empty:Function_info.empty

  let function_info_slot : (T.program, Function_info.t) KB.slot =
    KB.Class.property
      T.Program.cls "function-info" function_info_domain ~package

  let stash_function_info (label : T.label)
      (info : Function_info.t) : unit KB.t =
    KB.provide function_info_slot label info

  let get_function_info (label : T.label) : Function_info.t KB.t =
    KB.collect function_info_slot label

  let slot : (T.program, T.Semantics.t) KB.slot =
    KB.Class.property T.Program.cls "semantics"
      T.Semantics.domain ~package

  let set (label : T.label) (semantics : T.Semantics.t) : unit KB.t =
    KB.provide slot label semantics

end

let () =
  KB.promise T.Semantics.slot @@ fun label ->
  KB.collect Parsed_c_code.slot label
