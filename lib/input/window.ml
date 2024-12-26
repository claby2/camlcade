open Tsdl

type t = Sdl.Event.window_event_enum

let of_sdl_event e = Sdl.Event.(window_event_enum (get e window_event_id))
let compare = compare
