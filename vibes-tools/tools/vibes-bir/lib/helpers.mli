open Core
open Bap.Std
open Bap_core_theory

(** [create_sub name blks] creates a subroutine named [name] from
    a list of [blks]. It is implied that the first [blk] is the
    entry. If the list is empty, then an error is returned. *)
val create_sub :
  string ->
  blk term list ->
  sub term KB.t

(** [no_jmps blk] returns [true] if [blk] has no [jmp term]s.

    We say that such a block is an implicit exit node of the patch
    program.
*)
val no_jmps : blk term -> bool

(** Returns [true] if the jump is unconditional. *)
val is_unconditional : jmp term -> bool

(** Returns [true] if the jump is a call to a subroutine. *)
val is_call : jmp term -> bool

(** Returns [true] if the label refers to an indirect jump target. *)
val is_indirect_label : label -> bool

(** Returns [true] if the jump is a noreturn call, or returns to
    an indirect label. *)
val is_exit_call : jmp term -> bool

(** [exit_blks sub] finds the exit nodes of the patch code [sub].

    They are classified as follows:

    - [no_jmps]: This is implicitly an exit block since it has no
    successors in the CFG.

    - [goto <indirect>]: This block is explicitly jumping out of the
    program to somewhere else in the binary.

    - [when <c> ...]: If and only if this jump is the only one in
    the block. It is implicitly an exit because when [c] is false
    it will fall through.
   
    - [call <x> with noreturn]: This block must implicitly return to
    an exit block.

    - [call <x> with return <indirect>]: This block will not be returning
    to the patch code.
*)
val exit_blks : sub term -> (blk term list, KB.conflict) result

(** Returns [true] if the block contains a call to a subroutine. *)
val has_call : blk term -> bool

(** Returns the list of blocks in the subroutine that contain calls. *)
val call_blks : sub term -> blk term list

(** Returns the entry block of the subroutine, or an error if there
    are no blocks. *)
val entry_blk : sub term -> (blk term, KB.conflict) result

(** Equivalent to [Result.map (entry_blk sub) ~f:Term.tid]. *)
val entry_tid : sub term -> (tid, KB.conflict) result

(** Converts a BIL type into a Core Theory sort. *)
val sort_of_typ : typ -> unit Theory.Value.sort
