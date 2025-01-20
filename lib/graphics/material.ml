type t = {
  mutable ambient : Math.Vec3.t;
  mutable diffuse : Math.Vec3.t;
  mutable specular : Math.Vec3.t;
  mutable shininess : float;
}

let create ?(ambient = Math.Vec3.v 1. 1. 1.) ?(diffuse = Math.Vec3.v 1. 1. 1.)
    ?(specular = Math.Vec3.v 1. 1. 1.) ?(shininess = 0.0) () =
  { ambient; diffuse; specular; shininess }

let ambient t = t.ambient
let diffuse t = t.diffuse
let specular t = t.specular
let shininess t = t.shininess
let set_ambient t ambient = t.ambient <- ambient
let set_diffuse t diffuse = t.diffuse <- diffuse
let set_specular t specular = t.specular <- specular
let set_shininess t shininess = t.shininess <- shininess

module C = Ecs.Component.Make (struct
  type inner = t
end)
