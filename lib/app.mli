type t

val create : unit -> t
val add_plugin : (Ecs.World.t -> unit) -> t -> t
val run : t -> unit
