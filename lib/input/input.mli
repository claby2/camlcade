module Key_event = Key_event
module Keyboard = Keyboard
module Window_event : Ecs.Event.S with type event = Window.t

val plugin : Ecs.World.t -> unit
