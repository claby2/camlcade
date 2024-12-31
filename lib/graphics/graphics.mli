module Context = Context
module Camera = Camera
module Camera3d = Camera.Camera3d
module Mesh3d = Mesh3d
module Shader = Shader
module Vertex_mesh = Vertex_mesh
module Primitive = Primitive

val plugin : Ecs.World.t -> unit
(** Graphics plugin. *)
