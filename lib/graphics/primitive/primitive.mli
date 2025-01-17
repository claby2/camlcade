module Cuboid = Cuboid
module Sphere = Sphere

type t = float list

val to_vertex_mesh : t -> Vertex_mesh.t
val to_mesh3d : t -> Mesh3d.t
