open Tsdl

type t = Sdl.Event.window_event_enum

val of_sdl_event : Sdl.event -> t
val compare : t -> t -> int
