type t = {
  ambient : Math.Vec3.t;
  diffuse : Math.Vec3.t;
  specular : Math.Vec3.t;
  mutable shininess : float;
}

let create ?(ambient = Math.Vec3.v 1. 1. 1.) ?(diffuse = Math.Vec3.v 1. 1. 1.)
    ?(specular = Math.Vec3.v 1. 1. 1.) ?(shininess = 0.0) () =
  { ambient; diffuse; specular; shininess }

let ambient t = t.ambient
let diffuse t = t.diffuse
let specular t = t.specular
let shininess t = t.shininess
let set_shininess t shininess = t.shininess <- shininess

module C = Ecs.Component.Make (struct
  type inner = t
end)
