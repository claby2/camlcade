(** 3D mesh component. *)

type t

val of_vertex_mesh : Vertex_mesh.t -> t
(** Create a mesh component from a vertex mesh. *)

val vertex_mesh : t -> Vertex_mesh.t
(** Return the vertex mesh of the mesh component. *)

val initialize : t -> unit
(** Initialize the mesh component. If the mesh component is already initialized,
    this function does nothing. *)

val install : t -> unit
(** Install vertex data into the GPU. If the mesh component is not initialized,
    this function initializes it first.

    If the mesh component is already installed, this function re-installs it.
    This is useful when the underlying vertex data changes. *)

val draw : t -> unit
(** Draw the mesh component. This will initialize and install the mesh component
    as necessary. *)

val destroy : t -> unit
(** Destroy the mesh component. This will delete the vertex array and buffer
    objects. If the mesh component is not initialized, this function does
    nothing. *)

module C : Ecs.Component.S with type t = t
