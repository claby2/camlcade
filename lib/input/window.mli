(** Window events. *)

open Tsdl

type t = Sdl.Event.window_event_enum

val scan : Sdl.event -> t
