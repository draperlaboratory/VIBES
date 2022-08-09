(** A message. *)
type event = string

(** A handler for the message. *)
type observer = event -> unit

(** The formatter for messages. *)
type 'a formatter = ('a, Format.formatter, unit, unit) format4

(** Adds an observer to the logging stream. *)
val subscribe : observer -> unit

(** Formats and sends a message to all observers. *)
val send : 'a formatter -> 'a
