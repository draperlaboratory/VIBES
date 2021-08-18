(** Encapsulates higher variables for patches.

    A higher variable is a variable defined in a higher language,
    like in C, or in decompiled C-like code. Such higher variables
    have names (e.g., "x" or "my_counter"), and at the entry and
    exit of a patch, they are stored either in a register, or on
    the stack at some offset past the framepointer.
*)

open !Core_kernel

(** Some aliases for clarity. *)
type register = string
type offset = Bap.Std.Word.t

(** A higher variable is stored either in a register,
    or at some offset past the framepointer. *)
type stored_in =
  | Register of register
  | Memory of register * offset

(** A higher variable has a name, and a place where it is stored
    at the entrance and exit of the patch. *)
type t

(** Accessors. *)
val name : t -> string
val at_entry : t -> stored_in
val at_exit : t -> stored_in

(** [create name at_entry at_exit] creates a new higher variable record. *)
val create : register -> stored_in -> stored_in -> t

(** [equal t1 t2] checks if higher var records [t1] and [t2] are equal. *)
val equal : t -> t -> bool

(** Is the variable stored in a register? *)
val is_reg : stored_in -> bool

(** [find reg h_vars] finds the record for a higher variable named [reg]. *)
val find : register -> t list -> t option
