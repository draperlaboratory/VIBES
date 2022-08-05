open Bap.Std
open Bap_core_theory

(** Metadata for a particular function call.

    - [label]: the label representing the destination
      of the function call.
    - [name]: an optional name for the function.
    - [addr]: an optional address for the function.
    - [args]: a list of variables which serve as the
      arguments to the function.
*)
type func = {
  label : Vibes_utils_lib.Json.Label.t;
  name : string option;
  addr : Bitvec.t option;
  args : string list;
} [@@deriving yojson, equal, compare]

(** A list of function call metadata. *)
type t = {
  functions : func list;
} [@@deriving yojson, equal, compare]

(** Pretty-print the function call metadata. *)
val pp : Format.formatter -> t -> unit

(** [from_file filename] attempts to load the function
    information from [filename]. *)
val from_file : string -> (t, KB.conflict) result

(** Returns a string representation of the function
    info. *)
val to_string : t -> string

(** The empty list of function metadata. *)
val empty : t

(** [create_func tid args ?name ?addr] creates a function
    call record. *)
val create_func :
  ?name:string ->
  ?addr:Bitvec.t ->
  tid ->
  string list ->
  func

(** [create functions] creates the list of function calls. *)
val create : func list -> t

(** [append t ~func_info] appends [func_info] to [t]. *)
val append : t -> func_info:func -> t

(** [find_args t ~tid] finds the function associated with [tid],
     if it exists, and returns its list of argument vars. *)
val find_args : t -> tid:tid -> string list option KB.t
