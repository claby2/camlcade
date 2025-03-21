(** Bounce a ball up and down.

    Press `F` to follow the ball with the camera. Press `R` to restart the ball
    at the top. *)

open Camlcade
open Ecs

let maximum_height = 5.
let bounciness = 0.8
let ground = 0.5
let gravity = Math.Vec3.v 0. (-9.81) 0.

module Ball = struct
  type t = { mutable velocity : Math.Vec3.t; bounciness : float }

  let create bounciness () = { velocity = Math.Vec3.zero; bounciness }
  let velocity ball = ball.velocity
  let bounciness ball = ball.bounciness

  let fall ball dt =
    ball.velocity <- Math.Vec3.(add ball.velocity (smul dt gravity))

  let set_velocity ball v = ball.velocity <- v

  module C = Component.Make (struct
    type inner = t
  end)
end

let simulate_ball =
  let query w =
    let _, (t, (b, ())) =
      World.query w Query.[ Req (module Transform.C); Req (module Ball.C) ]
      |> List.hd
    in
    (t, b)
  in
  let dt = 0.00000005 in
  let simulate (transform, ball) =
    Ball.fall ball dt;
    let velocity = Ball.velocity ball in
    let translation = Transform.translation transform in
    let translation = Math.Vec3.(translation + velocity) in
    Transform.set_translation transform translation;

    if Math.Vec3.y translation < ground then (
      let x, _, z = Math.Vec3.to_tuple translation in
      let corrected_pos = Math.Vec3.v x ground z in
      Transform.set_translation transform corrected_pos;

      let bounce_y = -.Math.Vec3.y velocity *. Ball.bounciness ball in
      let vx, _, vz = Math.Vec3.to_tuple velocity in
      Ball.set_velocity ball (Math.Vec3.v vx bounce_y vz))
  in
  System.make query (System.Query simulate)

let restart_ball =
  let query w =
    let _, (k, ()) =
      World.query w Query.[ Req (module Input.Keyboard.C) ] |> List.hd
    in
    let _, (t, (b, ())) =
      World.query w Query.[ Req (module Transform.C); Req (module Ball.C) ]
      |> List.hd
    in
    (k, t, b)
  in
  let restart (keyboard, transform, ball) =
    let is_pressed = Input.Keyboard.is_pressed keyboard in
    if is_pressed `R then (
      Transform.set_translation transform (Math.Vec3.v 0. maximum_height 0.);
      Ball.set_velocity ball (Math.Vec3.v 0. 0. 0.))
  in
  System.make query (System.Query restart)

let camera_follow_ball =
  let query w =
    let _, (k, ()) =
      World.query w Query.[ Req (module Input.Keyboard.C) ] |> List.hd
    in
    let _, (ball_t, ()) =
      World.query ~filter:(Query.Filter.With Ball.C.id) w
        Query.[ Req (module Transform.C) ]
      |> List.hd
    in
    let _, (camera_t, ()) =
      World.query ~filter:(Query.Filter.With Graphics.Camera3d.C.id) w
        Query.[ Req (module Transform.C) ]
      |> List.hd
    in
    (k, ball_t, camera_t)
  in
  let follow = ref false in
  let follow (k, ball_t, camera_t) =
    if Input.Keyboard.is_just_pressed k `F then follow := not !follow;
    if !follow then
      let ball_position = Transform.translation ball_t in
      Transform.set_look_at camera_t ball_position
    else Transform.set_look_at camera_t Math.Vec3.oy
  in
  System.make query (System.Query follow)

let plugin w =
  let open Graphics in
  let _light =
    World.add_entity w
    |> World.with_component w
         (module Light.Point.C)
         (Light.Point.create ~color:(Math.Vec3.v 0.9 0.2 0.2)
            ~attenuation:(Math.Vec3.v 0.8 0.2 0.) ())
    |> World.with_component w
         (module Transform.C)
         Transform.(identity () |> with_translation (Math.Vec3.v 0. 5. 5.))
  in
  let _ground =
    World.add_entity w
    |> World.with_component w
         (module Mesh3d.C)
         (Primitive.to_mesh3d
            (Primitive.Cuboid.create ~x_length:100. ~y_length:0.1 ~z_length:100.
               ()))
    |> World.with_component w
         (module Material.C)
         (Material.create ~ambient:(Math.Vec3.v 0.2 0. 0.) ~shininess:1. ())
    |> World.with_component w (module Shader.Phong.C) ()
  in
  let _camera =
    World.add_entity w
    |> World.with_component w (module Camera3d.C) ()
    |> World.with_component w
         (module Camera.Projection.C)
         (Camera.Projection.perspective ())
    |> World.with_component w
         (module Transform.C)
         Transform.(
           identity ()
           |> with_translation (Math.Vec3.v (-15.) 1. 0.)
           |> with_look_at (Math.Vec3.v 0. 1. 0.))
  in
  let _ball =
    World.add_entity w
    |> World.with_component w
         (module Mesh3d.C)
         (Primitive.to_mesh3d
            (Primitive.Sphere.create ~param1:50 ~param2:50 ()))
    |> World.with_component w (module Shader.Phong.C) ()
    |> World.with_component w
         (module Material.C)
         (Material.create ~ambient:(Math.Vec3.v 0.1 0.5 0.2) ~shininess:1. ())
    |> World.with_component w
         (module Transform.C)
         Transform.(
           identity () |> with_translation (Math.Vec3.v 0. maximum_height 0.))
    |> World.with_component w (module Ball.C) (Ball.create bounciness ())
  in

  World.add_system w Scheduler.Update simulate_ball;
  World.add_system w Scheduler.Update restart_ball;
  World.add_system w Scheduler.Update camera_follow_ball

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
