module Dim3 = struct
  module T = struct
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

    let create ?(look = Math.Vec3.zero) ?(up = Math.Vec3.make 0. 1. 0.)
        ?(height_angle = 60.) ?(near_plane = 10.) ?(far_plane = 90.)
        ?(aspect_ratio = 4. /. 3.) pos =
      { pos; look; up; height_angle; near_plane; far_plane; aspect_ratio }

    let view t =
      let w = Math.Vec3.normalize (Math.Vec3.neg t.look) in
      let v =
        Math.Vec3.normalize
          Math.Vec3.(t.up - Math.Vec3.scale (Math.Vec3.dot t.up w) w)
      in
      let u = Math.Vec3.cross v w in
      let x, y, z = Math.Vec3.xyz v in
      let trans =
        Math.Mat4.from_array
          [|
            [| 1.; 0.; 0.; 0. |];
            [| 0.; 1.; 0.; 0. |];
            [| 0.; 0.; 1.; 0. |];
            [| -.x; -.y; -.z; 0. |];
          |]
      in
      let ux, uy, uz = Math.Vec3.xyz u in
      let vx, vy, vz = Math.Vec3.xyz v in
      let wx, wy, wz = Math.Vec3.xyz w in
      let rotate =
        Math.Mat4.from_array
          [|
            [| ux; vx; wx; 0. |];
            [| uy; vy; wy; 0. |];
            [| uz; vz; wz; 0. |];
            [| 0.; 0.; 0.; 1. |];
          |]
      in
      Math.Mat4.(rotate * trans)

    let projection t =
      let scale =
        Math.Mat4.from_array
          [|
            [|
              1. /. (t.aspect_ratio *. t.far_plane *. tan (t.height_angle /. 2.));
              0.;
              0.;
              0.;
            |];
            [| 0.; 1. /. (t.far_plane *. tan (t.height_angle /. 2.)); 0.; 0. |];
            [| 0.; 0.; 1. /. t.far_plane; 0. |];
            [| 0.; 0.; 0.; 1. |];
          |]
      in
      let c = -.t.near_plane /. t.far_plane in
      let unhinge =
        Math.Mat4.from_array
          [|
            [| 1.; 0.; 0.; 0. |];
            [| 0.; 1.; 0.; 0. |];
            [| 0.; 0.; 1. /. (1. +. c); -1. |];
            [| 0.; 0.; -.c /. (1. +. c); 0. |];
          |]
      in
      let adjust =
        Math.Mat4.from_array
          [|
            [| 1.; 0.; 0.; 0. |];
            [| 0.; 1.; 0.; 0. |];
            [| 0.; 0.; -2.; 0. |];
            [| 0.; 0.; -1.; 1. |];
          |]
      in
      Math.Mat4.(adjust * unhinge * scale)

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
