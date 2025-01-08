val frag : string
val vert : string

module C : Ecs.Component.S with type t = unit

type context

val query : Ecs.World.t -> context
val render : int -> context -> unit
