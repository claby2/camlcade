type t

val of_primitive : Primitive.t -> t
val of_vertex_mesh : Vertex_mesh.t -> t
val vertex_mesh : t -> Vertex_mesh.t
val initialize : t -> unit
val draw : t -> unit
val install_vbo : t -> unit
val destroy : t -> unit

module C : Ecs.Component.S with type t = t
