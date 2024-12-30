type perspective = {
  height_angle : float;
  near_plane : float;
  far_plane : float;
  aspect_ratio : float;
}

let project_perspective p =
  (* TODO: Clean up this code *)
  let scale =
    Math.Mat4.of_rows
      (Math.Vec4.v
         (1. /. (p.aspect_ratio *. p.far_plane *. tan (p.height_angle /. 2.)))
         0. 0. 0.)
      (Math.Vec4.v 0. (1. /. (p.far_plane *. tan (p.height_angle /. 2.))) 0. 0.)
      (Math.Vec4.v 0. 0. (1. /. p.far_plane) 0.)
      (Math.Vec4.v 0. 0. 0. 1.)
  in
  let c = -.p.near_plane /. p.far_plane in
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

(* TODO: Implement orthographic projection *)
type orthographic = unit

let project_orthographic _o = failwith "unimplemented"

type t = Perspective of perspective | Orthographic of orthographic

let perspective ?(near_plane = 0.1) ?(far_plane = 100.)
    ?(aspect_ratio = 4. /. 3.) ?(height_angle = Float.pi /. 6.) () =
  Perspective { height_angle; near_plane; far_plane; aspect_ratio }

let orthographic () = Orthographic ()

let project t =
  match t with
  | Perspective p -> project_perspective p
  | Orthographic o -> project_orthographic o

module C = Ecs.Component.Make (struct
  type inner = t
end)
