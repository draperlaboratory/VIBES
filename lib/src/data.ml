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
type cls
type t = cls KB.obj
let package = "vibes"
let name = "data"
let cls : (cls, unit) KB.cls = KB.Class.declare ~package name ()

(* Properties pertaining to the patch code *)
module Patch = struct

  (* Declare patches as a KB class.  A patch is a KB object of its own
     corresponding to a single code diff.  A run of the VIBES pipeline may
     involve multiple patches. *)
  type patch_cls
  let patch : (patch_cls, unit) KB.cls =
    KB.Class.declare ~package "patch" ()
  (* This provides equality / comparisons for objects of this class *)
  include (val KB.Object.derive patch)

  let patch_name : (patch_cls, string option) KB.slot =
    KB.Class.property ~package patch "patch-name" string_domain

  let patch_code : (patch_cls, string option) KB.slot =
    KB.Class.property ~package patch "patch-code" string_domain

  let bir : (patch_cls, Insn.t) KB.slot =
    KB.Class.property ~package patch "patch-bir" bir_domain

  let patch_point : (patch_cls, Bitvec.t option) KB.slot =
    KB.Class.property ~package patch "patch-point" bitvec_domain

  let patch_size : (patch_cls, int option) KB.slot =
    KB.Class.property ~package patch "patch-size" int_domain

  let assembly : (patch_cls, string list option) KB.slot =
    KB.Class.property ~package patch "patch-assembly" assembly_domain

  let set_patch_name (obj : t) (data : string option) : unit KB.t =
    KB.provide patch_name obj data

  let get_patch_name (obj : t) : string option KB.t =
    KB.collect patch_name obj

  let get_patch_name_exn (obj : t) : string KB.t =
    get_patch_name obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_patch_name
    | Some value -> KB.return value

  let set_patch_code (obj : t) (data : string option) : unit KB.t =
    KB.provide patch_code obj data

  let get_patch_code (obj : t) : string option KB.t =
    KB.collect patch_code obj

  let get_patch_code_exn (obj : t) : string KB.t =
    get_patch_code obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_patch_code
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

(* Sets of patches *)
module Patch_set = Set.Make (Patch)

(* Properties pertaining to the patched executable *)
module Patched_exe = struct
  let patch_domain : Patch_set.t KB.domain =
    KB.Domain.flat ~empty:Patch_set.empty ~equal:Patch_set.equal "patches"

  let patches : (cls, Patch_set.t) KB.slot =
    KB.Class.property ~package cls "patches" patch_domain

  let filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "patched-exe-filepath"
      string_domain

  let tmp_filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "tmp-patched-exe-filepath"
      string_domain

  let set_patches (obj : t) (data : Patch_set.t) : unit KB.t =
    KB.provide patches obj data

  let get_patches (obj : t) : (Patch_set.t) KB.t =
    KB.collect patches obj

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

end

(* Properties pertaining to the verifier *)
module Verifier = struct

  let property : (cls, Sexp.t option) KB.slot =
    KB.Class.property ~package cls "property" property_domain

  let func : (cls, string option) KB.slot =
    KB.Class.property ~package cls "func" string_domain

  let set_property (obj : t) (data : Sexp.t option) : unit KB.t =
    KB.provide property obj data

  let get_property (obj : t) : Sexp.t option KB.t =
    KB.collect property obj

  let get_property_exn (obj : t) : Sexp.t KB.t =
    get_property obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_property
    | Some value -> KB.return value

  let set_func (obj : t) (data : string option) : unit KB.t =
    KB.provide func obj data

  let get_func (obj : t) : string option KB.t =
    KB.collect func obj

  let get_func_exn (obj : t) : string KB.t =
    get_func obj >>= fun result ->
    match result with
    | None -> Errors.fail Errors.Missing_func
    | Some value -> KB.return value

end

(* Create an object of this class. *)
let create_patches (ps : Config.patch list) : Patch_set.t KB.t =
  let create_patch (p : Config.patch) : Patch.t KB.t =
    KB.Object.create Patch.patch >>= fun obj ->
    Patch.set_patch_name obj (Some p.patch_name) >>= fun () ->
    Patch.set_patch_code obj (Some p.patch_code) >>= fun () ->
    Patch.set_patch_point obj (Some p.patch_point) >>= fun () ->
    Patch.set_patch_size obj (Some p.patch_size) >>= fun () ->
    KB.return obj
  in
  KB.all (List.map ~f:create_patch ps) >>| Patch_set.of_list

let create (config : Config.t) : t KB.t =
  let exe = Config.exe config in
  let patch_list = Config.patches config in
  let func = Config.func config in
  let property = Config.property config in
  let patched_exe_filepath = Config.patched_exe_filepath config in
  create_patches patch_list >>= fun patches ->
  KB.Object.create cls >>= fun obj ->
  Original_exe.set_filepath obj (Some exe) >>= fun () ->
  Patched_exe.set_filepath obj patched_exe_filepath >>= fun () ->
  Patched_exe.set_patches obj patches >>= fun () ->
  Verifier.set_func obj (Some func) >>= fun () ->
  Verifier.set_property obj (Some property) >>= fun () ->
  KB.return obj

(* Create a fresh version of an object. *)
let fresh_patches (patches : Patch_set.t) : Patch_set.t KB.t =
  let fresh_patch (patch : Patch.t) : Patch.t KB.t =
    Patch.get_patch_name patch >>= fun name ->
    Patch.get_patch_code patch >>= fun code ->
    Patch.get_patch_point patch >>= fun point ->
    Patch.get_patch_size patch >>= fun size ->
    Patch.get_bir patch >>= fun bir ->
    KB.Object.create Patch.patch >>= fun patch' ->
    Patch.set_patch_name patch' name >>= fun () ->
    Patch.set_patch_point patch' point >>= fun () ->
    Patch.set_patch_code patch' code >>= fun () ->
    Patch.set_patch_size patch' size >>= fun () ->
    Patch.set_bir patch' bir >>= fun () ->
    KB.return patch'
  in
  KB.all (List.map ~f:fresh_patch (Patch_set.to_list patches)) >>=
    fun ps' -> KB.return (Patch_set.of_list ps')

let fresh ~property:(property : Sexp.t) (obj : t) : t KB.t =
  KB.Object.create cls >>= fun obj' ->
  Original_exe.get_filepath obj >>= fun original_exe ->
  Original_exe.set_filepath obj' original_exe >>= fun () ->
  Original_exe.get_prog obj >>= fun original_exe_prog ->
  Original_exe.set_prog obj' original_exe_prog >>= fun () ->
  Original_exe.get_addr_size obj >>= fun addr_size ->
  Original_exe.set_addr_size obj' addr_size >>= fun () ->
  Patched_exe.get_patches obj >>= fun patches ->
  fresh_patches patches >>= fun patches' ->
  Patched_exe.set_patches obj patches' >>= fun () ->
  Patched_exe.get_filepath obj >>= fun patched_exe ->
  Patched_exe.set_filepath obj patched_exe >>= fun () ->
  Verifier.get_func obj >>= fun func ->
  Verifier.set_func obj func >>= fun () ->
  Verifier.set_property obj' (Some property) >>= fun () ->
  KB.return obj'
