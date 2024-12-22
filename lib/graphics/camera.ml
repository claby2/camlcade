(*
module Dim3 = struct
  module T = struct
    type t = {
      mutable pos : Math.Vec3.t;
      mutable look : Math.Vec3.t;
      mutable up : Math.Vec3.t;
      mutable height_angle : float;
      mutable near_plane : float;
      mutable far_plane : float;
      mutable aspect_ratio : float;
    }

    let create ?(look = Math.Vec3.zero) ?(up = Math.Vec3.make 0. 1. 0.)
        ?(height_angle = 60.) ?(near_plane = 10.) ?(far_plane = 90.)
        ?(aspect_ratio = 4. /. 3.) pos =
      { pos; look; up; height_angle; near_plane; far_plane; aspect_ratio }

    let view _t = failwith "unimplemented"
    let projection _t = failwith "unimplemented"
    let look t = t.look
    let up t = t.up
    let set_pos t p = t.pos <- p
    let move_pos t delta = set_pos t Math.Vec3.(t.pos + delta)
    let rotate _t _angle _axis = failwith "unimplemented"
    let set_aspect_ratio t v = t.aspect_ratio <- v
    let set_near_plane t v = t.near_plane <- v
    let set_far_plane t v = t.far_plane <- v
  end

  module C = Ecs.Component.Make (T)
end
*)
