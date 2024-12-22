module T : sig
  type t

  val from_mesh : Mesh.t -> t
  val mesh : t -> Mesh.t
  val initialize : t -> unit
  val with_vao : t -> (unit -> unit) -> unit
  val draw : t -> unit
  val install_vbo : t -> unit
  val destroy : t -> unit
end

module C : Ecs.Component.S with type t = T.t
