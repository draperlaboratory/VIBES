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

(* The domain of BIR programs *)
let bir_domain : Insn.t KB.domain = Theory.Semantics.domain

(* Optional s-expression domain (e.g., for correctness properties) *)
let sexp_domain : Sexp.t option KB.Domain.t = KB.Domain.optional
    ~equal:Sexp.equal
    "sexp-domain"

(* Optional s-expression list domain (e.g., for patch code) *)
let sexp_list_domain : Sexp.t list option KB.Domain.t = KB.Domain.optional
    ~equal:(fun x y -> List.equal Sexp.equal x y)
    "sexp-list-domain"

(* Optional string list domain (for assembly). But as per
   Ivan's suggestion, we'll probably make this a powerset domain. *)
let assembly_domain : string list option KB.Domain.t = KB.Domain.optional
    ~equal:(fun x y -> List.equal String.equal x y)
    "assembly-domain"

(* Optional Ir domain for storing ir immediately after translation from core_theory *)
   let ir_domain : Ir.t option KB.Domain.t = KB.Domain.optional
   ~equal:Ir.equal
   "ir-domain"

(* For storing sets of minizinc solutions *)
let minizinc_solution_domain : Minizinc.sol_set KB.Domain.t =
  KB.Domain.powerset (module Minizinc.Sol) "minizinc-solution-domain"

(* General knowledge info for the package *)
type cls
type t = cls KB.obj
type computed = (cls, unit) KB.cls KB.value
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

  let patch_code : (patch_cls, Sexp.t list option) KB.slot =
    KB.Class.property ~package patch "patch-code" sexp_list_domain

  let bir : (patch_cls, Insn.t) KB.slot =
    KB.Class.property ~package patch "patch-bir" bir_domain

  let raw_ir : (patch_cls, Ir.t option) KB.slot =
    KB.Class.property ~package patch "patch-raw-ir" ir_domain

  let patch_point : (patch_cls, Bitvec.t option) KB.slot =
    KB.Class.property ~package patch "patch-point" bitvec_domain

  let patch_size : (patch_cls, int option) KB.slot =
    KB.Class.property ~package patch "patch-size" int_domain

  let assembly : (patch_cls, string list option) KB.slot =
    KB.Class.property ~package patch "patch-assembly" assembly_domain

  let lang : (patch_cls, Theory.language) KB.slot =
    KB.Class.property ~package patch "patch-language" Theory.Language.domain

  let target : (patch_cls, Theory.target) KB.slot =
    KB.Class.property ~package patch "patch-target" Theory.Target.domain

  let minizinc_solutions : (patch_cls, Minizinc.sol_set) KB.slot =
    KB.Class.property ~package patch "minizinc-solutions"
    minizinc_solution_domain

  let set_patch_name (obj : t) (data : string option) : unit KB.t =
    KB.provide patch_name obj data

  let get_patch_name (obj : t) : string option KB.t =
    KB.collect patch_name obj

  let get_patch_name_exn (obj : t) : string KB.t =
    get_patch_name obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_name
    | Some value -> KB.return value

  let set_patch_code (obj : t) (data : Sexp.t list option) : unit KB.t =
    KB.provide patch_code obj data

  let get_patch_code (obj : t) : Sexp.t list option KB.t =
    KB.collect patch_code obj

  let get_patch_code_exn (obj : t) : Sexp.t list KB.t =
    get_patch_code obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_code
    | Some value -> KB.return value

  let set_patch_point (obj : t) (data : Bitvec.t option) : unit KB.t =
    KB.provide patch_point obj data

  let get_patch_point (obj : t) : Bitvec.t option KB.t =
    KB.collect patch_point obj

  let get_patch_point_exn (obj : t) : Bitvec.t KB.t =
    get_patch_point obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_point
    | Some value -> KB.return value

  let set_patch_size (obj : t) (data : int option) : unit KB.t =
    KB.provide patch_size obj data

  let get_patch_size (obj : t) : int option KB.t =
    KB.collect patch_size obj

  let get_patch_size_exn (obj : t) : int KB.t =
    get_patch_size obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_patch_size
    | Some value -> KB.return value

  let set_bir (obj : t) (data : Insn.t) : unit KB.t =
    KB.provide bir obj data

  let get_bir (obj : t) : Insn.t KB.t =
    KB.collect bir obj

  let set_lang (obj : t) (data : Theory.language) : unit KB.t =
    KB.provide lang obj data

  let get_lang (obj : t) : Theory.language KB.t =
    KB.collect lang obj

  let set_target (obj : t) (data : Theory.target) : unit KB.t =
    KB.provide target obj data

  let get_target (obj : t) : Theory.target KB.t =
    KB.collect target obj

  let set_raw_ir (obj : t) (data : Ir.t option) : unit KB.t =
    KB.provide raw_ir obj data

  let get_raw_ir (obj : t) : Ir.t option KB.t =
    KB.collect raw_ir obj

  let get_raw_ir_exn (obj : t) : Ir.t KB.t =
    get_raw_ir obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_raw_ir
    | Some value -> KB.return value

  let set_assembly (obj : t) (data : string list option) : unit KB.t =
    KB.provide assembly obj data

  let get_assembly (obj : t) : string list option KB.t =
    KB.collect assembly obj

  let get_assembly_exn (obj : t) : string list KB.t =
    get_assembly obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_assembly
    | Some value -> KB.return value

  let get_minizinc_solutions (obj : t) : Minizinc.sol_set KB.t =
    KB.collect minizinc_solutions obj

  let add_minizinc_solution (obj : t) (sol : Minizinc.sol) : unit KB.t =
    KB.provide minizinc_solutions obj (Set.singleton (module Minizinc.Sol) sol)

  let union_minizinc_solution (obj : t) (sol_set : Minizinc.sol_set)
      : unit KB.t =
    KB.provide minizinc_solutions obj sol_set

end

(* Sets of patches *)
module Patch_set = Set.Make (Patch)

(* Properties pertaining to the original executable *)
module Original_exe = struct

  let filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "original-exe-filepath"
      string_domain

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
    | None -> Kb_error.fail Kb_error.Missing_original_exe_filepath
    | Some value -> KB.return value

  let set_addr_size (obj : t) (data : int option) : unit KB.t =
    KB.provide addr_size obj data

  let get_addr_size (obj : t) : int option KB.t =
    KB.collect addr_size obj

  let get_addr_size_exn (obj : t) : int KB.t =
    get_addr_size obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_addr_size
    | Some value -> KB.return value

end

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
    | None -> Kb_error.fail Kb_error.Missing_patched_exe_filepath
    | Some value -> KB.return value

  let set_tmp_filepath (obj : t) (data : string option) : unit KB.t =
    KB.provide tmp_filepath obj data

  let get_tmp_filepath (obj : t) : string option KB.t =
    KB.collect tmp_filepath obj

  let get_tmp_filepath_exn (obj : t) : string KB.t =
    get_tmp_filepath obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_tmp_patched_exe_filepath
    | Some value -> KB.return value

end

(* Properties pertaining to the solver *)
module Solver = struct

  let minizinc_model_filepath : (cls, string option) KB.slot =
    KB.Class.property ~package cls "minizinc-model-filepath" string_domain

  let set_minizinc_model_filepath (obj : t) (data : string option)
      : unit KB.t =
    KB.provide minizinc_model_filepath obj data

  let get_minizinc_model_filepath (obj : t) : string option KB.t =
    KB.collect minizinc_model_filepath obj

  let get_minizinc_model_filepath_exn (obj : t) : string KB.t =
    get_minizinc_model_filepath obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_minizinc_model_filepath
    | Some value -> KB.return value

end

(* Properties pertaining to the verifier *)
module Verifier = struct

  let property : (cls, Sexp.t option) KB.slot =
    KB.Class.property ~package cls "property" sexp_domain

  let func : (cls, string option) KB.slot =
    KB.Class.property ~package cls "func" string_domain

  let set_property (obj : t) (data : Sexp.t option) : unit KB.t =
    KB.provide property obj data

  let get_property (obj : t) : Sexp.t option KB.t =
    KB.collect property obj

  let get_property_exn (obj : t) : Sexp.t KB.t =
    get_property obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_property
    | Some value -> KB.return value

  let set_func (obj : t) (data : string option) : unit KB.t =
    KB.provide func obj data

  let get_func (obj : t) : string option KB.t =
    KB.collect func obj

  let get_func_exn (obj : t) : string KB.t =
    get_func obj >>= fun result ->
    match result with
    | None -> Kb_error.fail Kb_error.Missing_func
    | Some value -> KB.return value

end
