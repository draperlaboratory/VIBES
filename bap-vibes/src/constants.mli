val relative_patch_placement : string
(** [relative_patch_placement] is a variable .set in the assembly
    generated by the patcher.

    Its value is a relative address calculated by the actual patch
    placement location minus the location of of the code it is
    replacing (the "patch point"). *)

val patch_start_label : string
(** [patch_start_label] A label placed at the start of the patch
    assembly. Useful for addressing relative to the patch start. *)

val patch_location : string
(** [patch_location] A variable set in the assembly file with the the file
    offset in the binary at which the patch will be placed. *)
