type perspective = {
  height_angle : float;
  near_plane : float;
  far_plane : float;
  aspect_ratio : float;
}

let project_perspective p =
  let v = Math.Vec4.v in
  let scale =
    Math.Mat4.of_rows
      (v
         (1. /. (p.aspect_ratio *. p.far_plane *. tan (p.height_angle /. 2.)))
         0. 0. 0.)
      (v 0. (1. /. (p.far_plane *. tan (p.height_angle /. 2.))) 0. 0.)
      (v 0. 0. (1. /. p.far_plane) 0.)
      (v 0. 0. 0. 1.)
  in
  let c = -.p.near_plane /. p.far_plane in
  let unhinge =
    Math.Mat4.of_rows (v 1. 0. 0. 0.) (v 0. 1. 0. 0.)
      (v 0. 0. (1. /. (1. +. c)) (-.c /. (1. +. c)))
      (v 0. 0. (-1.) 0.)
  in
  let adjust =
    Math.Mat4.of_rows (v 1. 0. 0. 0.) (v 0. 1. 0. 0.) (v 0. 0. (-2.) (-1.))
      (v 0. 0. 0. 1.)
  in
  Math.Mat4.(mul (mul adjust unhinge) scale)

type orthographic = {
  left : float;
  right : float;
  bottom : float;
  top : float;
  near_plane : float;
  far_plane : float;
}

let project_orthographic o =
  let inv_rl = 1.0 /. (o.right -. o.left) in
  let inv_tb = 1.0 /. (o.top -. o.bottom) in
  let inv_fn = 1.0 /. (o.far_plane -. o.near_plane) in

  let v = Math.Vec4.v in
  Math.Mat4.of_rows
    (v (2. *. inv_rl) 0. 0. (-.(o.right +. o.left) *. inv_rl))
    (v 0. (2. *. inv_tb) 0. (-.(o.top +. o.bottom) *. inv_tb))
    (v 0. 0. (-2. *. inv_fn) (-.(o.far_plane +. o.near_plane) *. inv_fn))
    (v 0. 0. 0. 1.)

type t = Perspective of perspective | Orthographic of orthographic

let perspective ?(near_plane = 0.1) ?(far_plane = 100.)
    ?(aspect_ratio = 4. /. 3.) ?(height_angle = Float.pi /. 6.) () =
  Perspective { height_angle; near_plane; far_plane; aspect_ratio }

let orthographic ?(left = -1.) ?(right = 1.) ?(bottom = -1.) ?(top = 1.)
    ?(near_plane = 0.1) ?(far_plane = 100.) () =
  Orthographic { left; right; bottom; top; near_plane; far_plane }

let project t =
  match t with
  | Perspective p -> project_perspective p
  | Orthographic o -> project_orthographic o

module C = Ecs.Component.Make (struct
  type inner = t
end)
