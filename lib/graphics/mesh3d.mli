module T : sig
  type t

  val from_primitive : Primitive.t -> t
  val from_vertex_mesh : Vertex_mesh.t -> t
  val vertex_mesh : t -> Vertex_mesh.t
  val initialize : t -> unit
  val with_vao : t -> (unit -> unit) -> unit
  val draw : t -> unit
  val install_vbo : t -> unit
  val destroy : t -> unit
end

module C : Ecs.Component.S with type t = T.t
