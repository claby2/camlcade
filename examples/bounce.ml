(** Bounce a ball up and down. *)

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
  let query q =
    let open Query in
    let b = q (create [ Required Transform.C.id; Required Ball.C.id ]) in
    match b |> Result.single with
    | Some [ t; b ] ->
        ( t |> Component.unpack (module Transform.C),
          b |> Component.unpack (module Ball.C) )
    | _ -> assert false
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
  let query q =
    let open Query in
    let k = q (create [ Required Input.Keyboard.C.id ]) in
    let b = q (create [ Required Transform.C.id; Required Ball.C.id ]) in
    ( Result.as_single (module Input.Keyboard.C) k |> Option.get,
      match Result.single b with
      | Some [ t; b ] ->
          ( Component.unpack (module Transform.C) t,
            Component.unpack (module Ball.C) b )
      | _ -> assert false )
  in
  let restart (keyboard, (transform, ball)) =
    let is_pressed = Input.Keyboard.is_pressed keyboard in
    if is_pressed `R then (
      Transform.set_translation transform (Math.Vec3.v 0. maximum_height 0.);
      Ball.set_velocity ball (Math.Vec3.v 0. 0. 0.))
  in
  System.make query (System.Query restart)

let plugin w =
  let _ground =
    World.add_entity w
    |> World.with_component w
         (module Graphics.Mesh3d.C)
         (Graphics.Mesh3d.of_primitive
            (Graphics.Primitive.Cuboid.create ~x_length:100. ~y_length:0.1
               ~z_length:100. ()))
    |> World.with_component w (module Graphics.Shader.Normal.C) ()
  in
  let _camera =
    World.add_entity w
    |> World.with_component w (module Graphics.Camera3d.C) ()
    |> World.with_component w
         (module Graphics.Camera.Projection.C)
         (Graphics.Camera.Projection.perspective ())
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
         (module Graphics.Mesh3d.C)
         (Graphics.Mesh3d.of_primitive
            (Graphics.Primitive.Sphere.create ~param1:50 ~param2:50 ()))
    |> World.with_component w (module Graphics.Shader.Normal.C) ()
    |> World.with_component w
         (module Transform.C)
         Transform.(
           identity () |> with_translation (Math.Vec3.v 0. maximum_height 0.))
    |> World.with_component w (module Ball.C) (Ball.create bounciness ())
  in

  World.add_system w Scheduler.Update simulate_ball;
  World.add_system w Scheduler.Update restart_ball

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
