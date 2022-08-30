(** Removes all duplicates from the list according to [compare], while
    preserving the original ordering. *)
val dedup_list_stable : 'a list -> compare:('a -> 'a -> int) -> 'a list
