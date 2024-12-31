(** Input provides convenient components for handling input events. *)

open Tsdl
module Key_event : Ecs.Event.S with type event = Key.t
module Keyboard : Button_state.S with type button = Key.key
module Window_event : Ecs.Event.S with type event = Sdl.Event.window_event_enum

module Mouse : sig
  module Button_event : Ecs.Event.S with type event = Mouse_button.t
  module Button : Button_state.S with type button = Mouse_button.mouse_button

  type motion

  val x : motion -> int
  val y : motion -> int
  val dx : motion -> int
  val dy : motion -> int

  module Motion_event : Ecs.Event.S with type event = motion
end

val plugin : Ecs.World.t -> unit
