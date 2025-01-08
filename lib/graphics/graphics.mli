module Camera = Camera
module Camera3d = Camera.Camera3d
module Context = Context
module Light = Light
module Material = Material
module Mesh3d = Mesh3d
module Primitive = Primitive
module Shader = Shader
module Vertex_mesh = Vertex_mesh

val plugin : Ecs.World.t -> unit
(** Graphics plugin. *)
