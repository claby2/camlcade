(** Convenient keyboard input event component.

    This component reads key events from !{Input.Key_event} and makes it easy to
    query the state of keys. *)

type t

val create : unit -> t
(** Create a new keyboard input component. *)

val is_pressed : t -> Key.key -> bool
(** Check if a key is currently pressed. *)

val is_just_pressed : t -> Key.key -> bool
(** Check if a key was just pressed. *)

val is_just_released : t -> Key.key -> bool
(** Check if a key was just released. *)

module C : Ecs.Component.S with type t = t

val update_system : Ecs.World.t Ecs.System.t
