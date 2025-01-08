module Normal = Normal
module Phong = Phong

type t

val create : frag:string -> vert:string -> t
val initialize : t -> unit
val with_shader : t -> (int -> unit) -> unit
val destroy : t -> unit
val normal : t
val shade_normal : Ecs.World.t Ecs.System.t
val phong : t
val shade_phong : Ecs.World.t Ecs.System.t
