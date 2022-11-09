open Vibes_utils

(** A memory storage classifier.

    - [Frame]: a register that serves as a base address,
      plus an offset.

    - [Global]: a global variable, represented as an
      absolute address.
*)
type memory =
  | Frame of string * Json.Bitvector.t
  | Global of Json.Bitvector.t
[@@deriving yojson, equal, compare]

(** The value of a higher variable, which corresponds to
    what it will be substituted with and/or what storage
    classifications it has throughout the lifetime of the
    program.

    - [Constant]: the variable is a constant [c] and all uses
      of it shall be substituted with [c].

    - [Registers]: the variable lives in a register, and will
      optionally be in register [at_entry] at the start of the
      program, and optionally must be stored in [at_exit] when
      the program finishes normally. If [allow_opt] is [true]
      (default is [false]), then the variable is allowed to be
      optimized away during dead code elimination. Note that
      it will still be stored in [at_exit] if specified.

    - [Preassign]: the variable lives in a register and must
      be substituted at all uses and definitions with this
      register.

    - [Memory]: the variable lives in some memory location for
      the duration of the program.
*)
type value =
  | Constant of Json.Bitvector.t
  | Registers of {
      at_entry : string option;
      at_exit : string option;
      allow_opt : bool;
    } 
  | Preassign of string
  | Memory of memory
[@@deriving yojson, equal, compare]

type t = {
  name : string;
  value : value;
} [@@deriving yojson, equal, compare]

(** [find name hvars] tries to find the higher var [name] in
    [hvars]. *)
val find : string -> t list -> t option
