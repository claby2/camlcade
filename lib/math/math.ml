module Vec3 = struct
  include Gg.V3

  let normalize t = Gg.V3.smul (1. /. Gg.V3.norm t) t
end

module Vec4 = struct
  include Gg.V4

  let normalize t = Gg.V4.smul (1. /. Gg.V4.norm t) t
end

module Mat3 = struct
  include Gg.M3

  let to_list t =
    [
      Gg.M3.e00 t;
      Gg.M3.e10 t;
      Gg.M3.e20 t;
      Gg.M3.e01 t;
      Gg.M3.e11 t;
      Gg.M3.e21 t;
      Gg.M3.e02 t;
      Gg.M3.e12 t;
      Gg.M3.e22 t;
    ]
end

module Mat4 = struct
  include Gg.M4

  let to_list t =
    [
      Gg.M4.e00 t;
      Gg.M4.e10 t;
      Gg.M4.e20 t;
      Gg.M4.e30 t;
      Gg.M4.e01 t;
      Gg.M4.e11 t;
      Gg.M4.e21 t;
      Gg.M4.e31 t;
      Gg.M4.e02 t;
      Gg.M4.e12 t;
      Gg.M4.e22 t;
      Gg.M4.e32 t;
      Gg.M4.e03 t;
      Gg.M4.e13 t;
      Gg.M4.e23 t;
      Gg.M4.e33 t;
    ]
end

module Quat = struct
  include Gg.Quat

  let to_axes t =
    let x, y, z, w = Vec4.to_tuple t in
    let x2 = x +. x in
    let y2 = y +. y in
    let z2 = z +. z in
    let xx = x *. x2 in
    let xy = x *. y2 in
    let xz = x *. z2 in
    let yy = y *. y2 in
    let yz = y *. z2 in
    let zz = z *. z2 in
    let wx = w *. x2 in
    let wy = w *. y2 in
    let wz = w *. z2 in

    let x_axis = Vec4.v (1. -. (yy +. zz)) (xy +. wz) (xz -. wy) 0. in
    let y_axis = Vec4.v (xy -. wz) (1. -. (xx +. zz)) (yz +. wx) 0. in
    let z_axis = Vec4.v (xz +. wy) (yz -. wx) (1. -. (xx +. yy)) 0. in
    (x_axis, y_axis, z_axis)
end
