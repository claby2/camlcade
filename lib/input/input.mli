(** Input provides convenient components for handling input events. *)

open Tsdl
module Key_event : Ecs.Event.S with type event = Key.t
module Keyboard : Button_state.S with type button = Key.key
module Window_event : Ecs.Event.S with type event = Sdl.Event.window_event_enum

val plugin : Ecs.World.t -> unit
