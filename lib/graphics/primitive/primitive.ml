module Cuboid = Cuboid
module Sphere = Sphere

type t = float list

let to_vertex_mesh t =
  let vm = Vertex_mesh.create ~topology:TriangleList () in
  Vertex_mesh.set_attribute vm 0 3;
  Vertex_mesh.set_attribute vm 1 3;
  Vertex_mesh.set_data vm (Array.of_list t);
  vm

let to_mesh3d t = Mesh3d.of_vertex_mesh (to_vertex_mesh t)
