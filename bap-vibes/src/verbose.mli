(***************************************************************************)
(*                                                                         *)
(*  Copyright (C) 2022/2023 The Charles Stark Draper Laboratory, Inc.      *)
(*                                                                         *)
(*  This file is provided under the license found in the LICENSE file in   *)
(*  the top-level directory of this project.                               *)
(*                                                                         *)
(*  This research was developed with funding from the Defense Advanced     *)
(*  Research Projects Agency (DARPA).                                      *)
(*                                                                         *)
(***************************************************************************)

(** A module to handle verbose logging.

    The verbose log can subscribe to the {!Events} stream, and report all
    events it is notified of.

    The verbose logger here is {!Stderr}, which will report messages to
    stderr. It is parameterized by a simple {!Config} module, which is used
    to turn on/off colors in {!Stderr}'s output.

    The {!Stderr.handle} function is a handler that can be registered with
    the {!Events} stream, using {!Events.subscribe}. *)

(** A simple module that is used to configure the {!Stderr} module. *)
module type Config = sig
  val with_colors : bool
end

(** A module that reports events to stderr. The [handle] function can be
    registered as an observer of the {!Events} stream. If it is subscribed
    to the {!Events} stream, it will report all events it is notified of
    to stderr. It will use colors if [Conf] has [with_colors = true]. *)
module Stderr : functor (Conf : Config) -> sig
  val handle : Events.t -> unit
end
