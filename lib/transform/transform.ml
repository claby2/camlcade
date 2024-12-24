type t = {
  mutable translation : Math.Vec3.t;
  mutable rotation : Math.Quat.t;
  mutable scale : Math.Vec3.t;
}

let identity () =
  {
    translation = Math.Vec3.zero;
    rotation = Math.Quat.id;
    scale = Math.Vec3.v 1. 1. 1.;
  }

let translation t = t.translation
let rotation t = t.rotation
let scale t = t.scale
let set_translation t v = t.translation <- v
let set_rotation t q = t.rotation <- q
let set_scale t v = t.scale <- v

let compute_matrix t =
  let x_axis, y_axis, z_axis = Math.Quat.to_axes t.rotation in
  let sx, sy, sz = Math.Vec3.to_tuple t.scale in
  let tx, ty, tz = Math.Vec3.to_tuple t.translation in
  Math.Mat4.of_cols (Math.Vec4.smul sx x_axis) (Math.Vec4.smul sy y_axis)
    (Math.Vec4.smul sz z_axis) (Math.Vec4.v tx ty tz 1.)

module C = Ecs.Component.Make (struct
  type inner = t
end)
