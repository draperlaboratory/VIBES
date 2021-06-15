(** This module captures our KB "ontology" - the collection of classes and
    properties that the VIBES toolchain defines and manipulates *)

open !Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

module KB = Knowledge

(** We define "domains" for the types used in our properties. *)
val string_domain    : string option KB.Domain.t
val int_domain       : int option KB.Domain.t
val bitvec_domain    : Bitvec.t option KB.Domain.t
val sexp_domain      : Sexp.t option KB.Domain.t
val sexp_list_domain : Sexp.t list option KB.Domain.t
val assembly_domain  : string list option KB.Domain.t
val unit_domain      : unit KB.Domain.t

(** These are the top-level class definitions.

    - type [cls] is the class of the top-level VIBES KB object that stores
      all data related to a run in properties.
    - type [t] is the type of objects of that class.
    - val [cls] is the declared class.
    - type [computed] is the type of the result computed by [KB.run]. *)
type cls
type t = cls KB.obj
type computed = (cls, unit) KB.cls KB.value

val package : string
val name : string
val cls : (cls, unit) KB.cls

(** Register a generic promise contigent on the creation of a data class. *)
val promise : (t -> unit KB.t) -> unit

(** Trigger all the generic promises registered with [promise] *)
val force : t -> unit KB.t

(** The patch module defines an additional class holding all properties
    related to a specific patch fragment - a contiguous region of code that
    is being patched into the binary. The outer class defined above holds a
    collection of these (see {!Patched_exe.patches}). *)
module Patch : sig

  (* The KB infrastructure *)
  type patch_cls
  type t = patch_cls KB.obj
  val patch : (patch_cls, unit) KB.cls

  (* equal, compare, and other things for patches *)
  include Knowledge.Object.S with type t := t

  val patch_name : (patch_cls, string option) KB.slot
  val patch_code : (patch_cls, Sexp.t list option) KB.slot
  val patch_point : (patch_cls, Bitvec.t option) KB.slot
  val patch_size : (patch_cls, int option) KB.slot
  val patch_label : (patch_cls, Theory.label option) KB.slot
  val raw_ir : (patch_cls, Ir.t option) KB.slot
  val assembly : (patch_cls, string list option) KB.slot
  (* The language/encoding of the assembly, typically used to
     distinguish between ARM and Thumb. *)
  (* TODO: add the target as well. *)
  val lang : (patch_cls, Theory.language) KB.slot
  val target : (patch_cls, Theory.target) KB.slot
  val minizinc_solutions : (patch_cls, Minizinc.sol_set) KB.slot

  val set_patch_name : t -> string option -> unit KB.t
  val get_patch_name : t -> string option KB.t
  val get_patch_name_exn : t -> string KB.t

  val set_patch_code : t -> Sexp.t list option -> unit KB.t
  val get_patch_code : t -> Sexp.t list option KB.t
  val get_patch_code_exn : t -> Sexp.t list KB.t

  val set_patch_point : t -> Bitvec.t option -> unit KB.t
  val get_patch_point : t -> Bitvec.t option KB.t
  val get_patch_point_exn : t -> Bitvec.t KB.t

  val set_patch_size : t -> int option -> unit KB.t
  val get_patch_size : t -> int option KB.t
  val get_patch_size_exn : t -> int KB.t

  val init_sem : t -> unit KB.t
  val set_bir : t -> insn -> unit KB.t
  val get_bir : t -> insn KB.t

  val set_raw_ir : t -> Ir.t option -> unit KB.t
  val get_raw_ir : t -> Ir.t option KB.t
  val get_raw_ir_exn : t -> Ir.t KB.t

  val set_assembly : t -> string list option -> unit KB.t
  val get_assembly : t -> string list option KB.t
  val get_assembly_exn : t -> string list KB.t

  val set_lang : t -> Theory.language -> unit KB.t
  val get_lang : t -> Theory.language KB.t

  val set_target : t -> Theory.target -> unit KB.t
  val get_target : t -> Theory.target KB.t

  val get_minizinc_solutions : t -> Minizinc.sol_set KB.t
  val add_minizinc_solution : t -> Minizinc.sol -> unit KB.t
  val union_minizinc_solution : t -> Minizinc.sol_set -> unit KB.t

end

(** Sets of patches *)
module Patch_set : Set.S with type Elt.t = Patch.t

(** Properties pertaining to the original executable *)
module Original_exe : sig
  val filepath : (cls, string option) KB.slot
  val addr_size : (cls, int option) KB.slot

  val set_filepath : t -> string option -> unit KB.t
  val get_filepath : t -> string option KB.t
  val get_filepath_exn : t -> string KB.t

  val set_addr_size : t -> int option -> unit KB.t
  val get_addr_size : t -> int option KB.t
  val get_addr_size_exn : t -> int KB.t

end

(** Properties pertaining to the patched executable *)
module Patched_exe : sig

  val patches : (cls, Patch_set.t) KB.slot
  val filepath : (cls, string option) KB.slot
  val tmp_filepath : (cls, string option) KB.slot

  val set_patches : t -> Patch_set.t -> unit KB.t
  val get_patches : t -> Patch_set.t KB.t

  val set_filepath : t -> string option -> unit KB.t
  val get_filepath : t -> string option KB.t
  val get_filepath_exn : t -> string KB.t

  val set_tmp_filepath : t -> string option -> unit KB.t
  val get_tmp_filepath : t -> string option KB.t
  val get_tmp_filepath_exn : t -> string KB.t
end

(** Properties pertaining to the solver *)
module Solver : sig
  val minizinc_model_filepath : (cls, string option) KB.slot
  val set_minizinc_model_filepath : t -> string option -> unit KB.t
  val get_minizinc_model_filepath : t -> string option KB.t
  val get_minizinc_model_filepath_exn : t -> string KB.t
end

(** Properties pertaining to the verifier *)
module Verifier : sig
  val set_property : t -> Sexp.t option -> unit KB.t
  val get_property : t -> Sexp.t option KB.t
  val get_property_exn : t -> Sexp.t KB.t
  val set_func : t -> string option -> unit KB.t
  val get_func : t -> string option KB.t
  val get_func_exn : t -> string KB.t
end
