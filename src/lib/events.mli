(* An event stream for the VIBES pipeline.

   This module provides an event stream that modules in the VIBES pipeline
   can send messages to, for logging/communication. 

   If the user has enabled verbose logging, the {!Verbose} module will
   subscribe to this stream, and report all events. *)

(* Different types of messages that can be sent to the stream. *)
type t =
  | Header of string (* A heading/new section. *)
  | Info of string   (* An informational message. *)
  | Rule             (* A break/separator. *)

(* [subscribe handler] will register the function [handler] as an observer
   of the stream. Any events that come down the stream will then be passed
   to [handler]. *)
val subscribe : (t -> unit) -> unit

(* [send msg] will send a message to the stream. For example,
   [send (Header "foo")] sends the heading ["foo"], while
   [send (Info "bar")] sends the info ["bar"]. *)
val send : t -> unit
