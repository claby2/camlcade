type tag = Phong | Custom of string
type t

val create : frag:string -> vert:string -> string -> t
val phong : t
val tag_opt : t -> tag option
val initialize : t -> unit
val with_shader : t -> (int -> unit) -> unit
val destroy : t -> unit

module C : Ecs.Component.S with type t = t
