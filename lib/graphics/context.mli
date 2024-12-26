type t

val empty : unit -> t
val initialize : gl:int * int -> t -> unit
val get_window_size : t -> int * int
val render : t -> unit
val destroy : t -> unit

module C : Ecs.Component.S with type t = t
