open Tsdl

type t

val empty : unit -> t
val initialize : gl:int * int -> t -> unit
val get_window_size : t -> int * int
val render : t -> unit
val destroy : t -> unit

module Window = Sdl.Window

val set_window_fullscreen : t -> Window.flags -> unit
(** Set the window fullscreen. *)

val set_relative_mouse_mode : bool -> unit
(** Set the relative mouse mode. *)

module C : Ecs.Component.S with type t = t
