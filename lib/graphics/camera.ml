let radians_of_deg deg = deg *. Float.pi /. 180.

module Dim3 = struct
  type t = {
    mutable pos : Math.Vec3.t;
    (* TODO: Make look, up and height_angle mutable *)
    look : Math.Vec3.t;
    up : Math.Vec3.t;
    height_angle : float;
    mutable near_plane : float;
    mutable far_plane : float;
    mutable aspect_ratio : float;
  }

  let create ?(pos = Math.Vec3.zero) ?(look = Math.Vec3.zero)
      ?(up = Math.Vec3.v 0. 1. 0.) ?(height_angle = radians_of_deg 30.)
      ?(near_plane = 0.1) ?(far_plane = 100.) ?(aspect_ratio = 4. /. 3.) () =
    { pos; look; up; height_angle; near_plane; far_plane; aspect_ratio }

  let view t =
    let w = Math.Vec3.(normalize (neg t.look)) in
    let v = Math.Vec3.(normalize (t.up - smul (dot t.up w) w)) in
    let u = Math.Vec3.cross v w in
    let x, y, z = Math.Vec3.to_tuple t.pos in
    let trans =
      Math.Mat4.of_rows
        (Math.Vec4.v 1. 0. 0. (-.x))
        (Math.Vec4.v 0. 1. 0. (-.y))
        (Math.Vec4.v 0. 0. 1. (-.z))
        (Math.Vec4.v 0. 0. 0. 1.)
    in
    let ux, uy, uz = Math.Vec3.to_tuple u in
    let vx, vy, vz = Math.Vec3.to_tuple v in
    let wx, wy, wz = Math.Vec3.to_tuple w in
    let rotate =
      Math.Mat4.of_rows (Math.Vec4.v ux uy uz 0.) (Math.Vec4.v vx vy vz 0.)
        (Math.Vec4.v wx wy wz 0.) (Math.Vec4.v 0. 0. 0. 1.)
    in
    Math.Mat4.mul rotate trans

  let projection t =
    let scale =
      Math.Mat4.of_rows
        (Math.Vec4.v
           (1. /. (t.aspect_ratio *. t.far_plane *. tan (t.height_angle /. 2.)))
           0. 0. 0.)
        (Math.Vec4.v 0.
           (1. /. (t.far_plane *. tan (t.height_angle /. 2.)))
           0. 0.)
        (Math.Vec4.v 0. 0. (1. /. t.far_plane) 0.)
        (Math.Vec4.v 0. 0. 0. 1.)
    in
    let c = -.t.near_plane /. t.far_plane in
    let unhinge =
      Math.Mat4.of_rows (Math.Vec4.v 1. 0. 0. 0.) (Math.Vec4.v 0. 1. 0. 0.)
        (Math.Vec4.v 0. 0. (1. /. (1. +. c)) (-.c /. (1. +. c)))
        (Math.Vec4.v 0. 0. (-1.) 0.)
    in
    let adjust =
      Math.Mat4.of_rows (Math.Vec4.v 1. 0. 0. 0.) (Math.Vec4.v 0. 1. 0. 0.)
        (Math.Vec4.v 0. 0. (-2.) (-1.))
        (Math.Vec4.v 0. 0. 0. 1.)
    in
    Math.Mat4.mul (Math.Mat4.mul adjust unhinge) scale

  let look t = t.look
  let up t = t.up
  let set_pos t p = t.pos <- p
  let move_pos t delta = set_pos t Math.Vec3.(t.pos + delta)
  let rotate _t _angle _axis = failwith "unimplemented"
  let set_aspect_ratio t v = t.aspect_ratio <- v
  let set_near_plane t v = t.near_plane <- v
  let set_far_plane t v = t.far_plane <- v

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end
