module Cuboid = Cuboid
module Sphere = Sphere

type t = Shape.t

let positions t = Shape.(t.positions)
let normals t = Shape.(t.normals)
