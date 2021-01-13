(* This module currently encapsulates our KB ontology.
   We expect much evolution here... *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory
open Knowledge.Syntax

module KB = Knowledge

(* Optional string domain *)
let string_domain : String.t option KB.Domain.t = KB.Domain.optional
    ~equal:String.(=)
    "string-domain"

(* Optional int domain *)
let int_domain : int option KB.Domain.t = KB.Domain.optional
    ~equal:Int.(=)
    "int-domain"

(* Optional bitvector domain *)
let bitvec_domain : Bitvec.t option KB.Domain.t = KB.Domain.optional
    ~equal:Bitvec.equal
    "bitvec-domain"

(* Optional program domain *)
(* FIXME: this is confusing, since there is a program class, which
   also has an associated domain *)
let prog_domain : Program.t option KB.Domain.t = KB.Domain.optional
    ~equal:Program.equal
    "prog-domain"

(* The domain of BIR programs *)
let bir_domain : Insn.t KB.domain = Theory.Semantics.domain

(* Optional s-expression domain (for correctness properties) *)
let property_domain : Sexp.t option KB.Domain.t = KB.Domain.optional
    ~equal:Sexp.equal
    "property-domain"

(* Optional string list domain (for assembly). But as per
   Ivan's suggestion, we'll probably make this a powerset domain. *)
let assembly_domain : string list option KB.Domain.t = KB.Domain.optional
    ~equal:(fun x y -> List.equal String.equal x y)
    "assembly-domain"

(* General knowledge info for the package *)
type cls = Data
type t = cls KB.obj
let package = "vibes"
let name = "data"
let cls : (cls, unit) KB.cls = KB.Class.declare ~package name ()

(* Properties pertaining to the patch code *)
module Patch = struct

  let patch_name : (cls, string option) KB.slot =
    KB.Class.property ~package cls "patch-name" string_domain

  let bir : (cls, Insn.t) KB.slot =
    KB.Class.property ~package cls "patch-bir" bir_domain

  let assembly : (cls, string list option) KB.slot =
    KB.Class.property ~package cls "patch-assembly" assembly_domain

  let set_patch_name (obj : t) (data : string option) : unit KB.t =
    KB.provide patch_name obj data

  let get_patch_name (obj : t) : string option KB.t =
    KB.collect patch_name obj

  let get_patch_name_exn (obj : t) : string KB.t =
    get_patch_name obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_patch_name
    | Some value -> KB.return value

  let set_bir (obj : t) (data : Insn.t) : unit KB.t =
    KB.provide bir obj data

  let get_bir (obj : t) : Insn.t KB.t =
    KB.collect bir obj

  let set_assembly (obj : t) (data : string list option) : unit KB.t =
    KB.provide assembly obj data

  let get_assembly (obj : t) : string list option KB.t =
    KB.collect assembly obj

  let get_assembly_exn (obj : t) : string list KB.t =
    get_assembly obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_assembly
    | Some value -> KB.return value

end

(* Properties pertaining to the original executable *)
module Original_exe = struct

  let filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "original-exe-filepath"
      string_domain

  let prog : (cls, Program.t option) KB.slot =
    KB.Class.property ~package cls "original-exe-prog"
      prog_domain

  let addr_size : (cls, int option) KB.slot =
    KB.Class.property ~package cls "original-exe-address-size"
      int_domain

  let set_filepath (obj : t) (data : string option) : unit KB.t =
    KB.provide filepath obj data

  let get_filepath (obj : t) : string option KB.t =
    KB.collect filepath obj

  let get_filepath_exn (obj : t) : string KB.t =
    get_filepath obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_original_exe_filepath
    | Some value -> KB.return value

  let set_prog (obj : t) (data : Program.t option) : unit KB.t =
    KB.provide prog obj data

  let get_prog (obj : t) : Program.t option KB.t =
    KB.collect prog obj

  let get_prog_exn (obj : t) : Program.t KB.t =
    get_prog obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_original_exe_prog
    | Some value -> KB.return value

  let set_addr_size (obj : t) (data : int option) : unit KB.t =
    KB.provide addr_size obj data

  let get_addr_size (obj : t) : int option KB.t =
    KB.collect addr_size obj

  let get_addr_size_exn (obj : t) : int KB.t =
    get_addr_size obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_addr_size
    | Some value -> KB.return value

end

(* Properties pertaining to the patched executable *)
module Patched_exe = struct

  let filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "patched-exe-filepath"
      string_domain

  let tmp_filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "tmp-patched-exe-filepath"
      string_domain

  let patch_point : (cls, Bitvec.t option) KB.slot =
    KB.Class.property ~package cls "patch-point" bitvec_domain

  let patch_size : (cls, int option) KB.slot =
    KB.Class.property ~package cls "patch-size" int_domain

  let set_filepath (obj : t) (data : string option) : unit KB.t =
    KB.provide filepath obj data

  let get_filepath (obj : t) : string option KB.t =
    KB.collect filepath obj

  let get_filepath_exn (obj : t) : string KB.t =
    get_filepath obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_patched_exe_filepath
    | Some value -> KB.return value

  let set_tmp_filepath (obj : t) (data : string option) : unit KB.t =
    KB.provide tmp_filepath obj data

  let get_tmp_filepath (obj : t) : string option KB.t =
    KB.collect tmp_filepath obj

  let get_tmp_filepath_exn (obj : t) : string KB.t =
    get_tmp_filepath obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_tmp_patched_exe_filepath
    | Some value -> KB.return value

  let set_patch_point (obj : t) (data : Bitvec.t option) : unit KB.t =
    KB.provide patch_point obj data

  let get_patch_point (obj : t) : Bitvec.t option KB.t =
    KB.collect patch_point obj

  let get_patch_point_exn (obj : t) : Bitvec.t KB.t =
    get_patch_point obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_patch_point
    | Some value -> KB.return value

  let set_patch_size (obj : t) (data : int option) : unit KB.t =
    KB.provide patch_size obj data

  let get_patch_size (obj : t) : int option KB.t =
    KB.collect patch_size obj

  let get_patch_size_exn (obj : t) : int KB.t =
    get_patch_size obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_patch_size
    | Some value -> KB.return value

end

(* Properties pertaining to the verifier *)
module Verifier = struct

  let property : (cls, Sexp.t option) KB.slot =
    KB.Class.property ~package cls "property" property_domain

  let set_property (obj : t) (data : Sexp.t option) : unit KB.t =
    KB.provide property obj data

  let get_property (obj : t) : Sexp.t option KB.t =
    KB.collect property obj

  let get_property_exn (obj : t) : Sexp.t KB.t =
    get_property obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_property
    | Some value -> KB.return value

end

(* Create an object of this class. *)
let create (config : Config.t) : t KB.t =
  let exe = Config.exe config in
  let patch = Config.patch config in
  let patch_point = Config.patch_point config in
  let patch_size = Config.patch_size config in
  let property = Config.property config in
  let patched_exe_filepath = Config.patched_exe_filepath config in
  KB.Object.create cls >>= fun obj ->
  Original_exe.set_filepath obj (Some exe) >>= fun _ ->
  Patch.set_patch_name obj (Some patch) >>= fun _ ->
  Patched_exe.set_filepath obj patched_exe_filepath >>= fun _ ->
  Patched_exe.set_patch_point obj (Some patch_point) >>= fun _ ->
  Patched_exe.set_patch_size obj (Some patch_size) >>= fun _ ->
  Verifier.set_property obj (Some property) >>= fun _ ->
  KB.return obj

(* Create a fresh version of an object. *)
let fresh ~property:(property : Sexp.t) (obj : t) : t KB.t =
  KB.Object.create cls >>= fun obj' ->
  Patch.get_patch_name obj >>= fun patch ->
  Patch.set_patch_name obj' patch >>= fun _ ->
  Patch.get_bir obj >>= fun bir ->
  Patch.set_bir obj' bir >>= fun _ ->
  Original_exe.get_filepath obj >>= fun original_exe ->
  Original_exe.set_filepath obj' original_exe >>= fun _ ->
  Original_exe.get_prog obj >>= fun original_exe_prog ->
  Original_exe.set_prog obj' original_exe_prog >>= fun _ ->
  Original_exe.get_addr_size obj >>= fun addr_size ->
  Original_exe.set_addr_size obj' addr_size >>= fun _ ->
  Patched_exe.get_filepath obj >>= fun patched_exe ->
  Patched_exe.set_filepath obj patched_exe >>= fun _ ->
  Patched_exe.get_patch_point obj >>= fun patch_point ->
  Patched_exe.set_patch_point obj' patch_point >>= fun _ ->
  Patched_exe.get_patch_size obj >>= fun patch_size ->
  Patched_exe.set_patch_size obj' patch_size >>= fun _ ->
  Verifier.set_property obj' (Some property) >>= fun _ ->
  KB.return obj'
