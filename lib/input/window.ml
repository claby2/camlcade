open Tsdl

type t = Sdl.Event.window_event_enum

let scan e = Sdl.Event.(window_event_enum (get e window_event_id))
