
(**
[relative_patch_placement] is a variable .set in the assembly generated 
by the patcher to a relative address calculated by the actual patch placement location minus the
location of of the code it is replacing (the "patch point").
The variable is 

*)
val relative_patch_placement : string

(**
 [patch_start_label] A label placed at the start of the patch assembly. Useful for
 addressing relative to the patch start.
*)
val patch_start_label : string