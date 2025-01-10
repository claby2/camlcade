open Camlcade
open Ecs

module Spin = struct
  module C = Ecs.Component.Make (struct
    type inner = unit
  end)
end

module Disco_ball = struct
  type t = {
    mutable last_update : float;
    mutable color : Math.Vec3.t;
    mutable target_color : Math.Vec3.t;
  }

  let update_interval = 1.0

  let random_color () =
    Math.Vec3.v (Random.float 1.) (Random.float 1.) (Random.float 1.)

  let create ?(target_color = random_color ()) () =
    { last_update = 0.; color = random_color (); target_color }

  let update t now =
    if now -. t.last_update > update_interval then (
      t.target_color <- random_color ();
      t.last_update <- now;
      t.target_color)
    else
      let lerp a b = a +. ((b -. a) *. 0.001) in
      let r, g, b = Math.Vec3.to_tuple t.color in
      let tr, tg, tb = Math.Vec3.to_tuple t.target_color in
      t.color <- Math.Vec3.v (lerp r tr) (lerp g tg) (lerp b tb);
      t.color

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end

let spawn_camera w =
  let open Graphics in
  World.add_entity w
  |> World.with_component w (module Camera3d.C) ()
  |> World.with_component w
       (module Camera.Projection.C)
       (Camera.Projection.perspective ~far_plane:1000. ())
  |> World.with_component w
       (module Transform.C)
       Transform.(
         identity ()
         |> with_translation (Math.Vec3.v (-30.) 10. 0.)
         |> with_look_at (Math.Vec3.v 0. 1. 0.))
  |> ignore

let spawn_disco_ball w position =
  let open Graphics in
  World.add_entity w
  |> World.with_component w (module Disco_ball.C) (Disco_ball.create ())
  |> World.with_component w
       (module Transform.C)
       Transform.(identity () |> with_translation position)
  |> World.with_component w (module Light.Point.C) (Light.Point.create ())
  |> ignore

let spawn_cube w position =
  let open Graphics in
  World.add_entity w
  |> World.with_component w (module Spin.C) ()
  |> World.with_component w
       (module Mesh3d.C)
       (Mesh3d.of_primitive (Primitive.Cuboid.create ()))
  |> World.with_component w
       (module Transform.C)
       Transform.(identity () |> with_translation position)
  |> World.with_component w
       (module Material.C)
       (Material.create ~ambient:(Math.Vec3.v 0. 0. 0.) ~shininess:1. ())
  |> World.with_component w (module Shader.Phong.C) ()
  |> ignore

let spawn_ground w =
  let open Graphics in
  World.add_entity w
  |> World.with_component w
       (module Mesh3d.C)
       (Mesh3d.of_primitive
          (Primitive.Cuboid.create ~x_length:25. ~y_length:0.1 ~z_length:25. ()))
  |> World.with_component w
       (module Material.C)
       (Material.create ~ambient:(Math.Vec3.v 0.1 0.1 0.1) ~shininess:1. ())
  |> World.with_component w (module Shader.Phong.C) ()
  |> ignore

let spin =
  let query w =
    World.query ~filter:(Query.Filter.With Spin.C.id) w
      Query.(Req (module Transform.C) @ Nil)
    |> List.map (fun (_, (t, ())) -> t)
  in
  let spin =
    List.iter (fun t ->
        Transform.set_rotation t
          (Math.Quat.rot3_zyx
             (Math.Vec3.v (Unix.gettimeofday ()) (Unix.gettimeofday ())
                (Unix.gettimeofday ()))))
  in
  System.make query (System.Query spin)

let update_disco_balls =
  let open Graphics in
  let query w =
    World.query w
      Query.(Req (module Light.Point.C) @ Req (module Disco_ball.C) @ Nil)
    |> List.map (fun (_, (p, (db, ()))) -> (p, db))
  in
  let dim_color color =
    let dim_factor = 0.2 in
    Math.Vec3.smul dim_factor color
  in
  let update =
    List.iter (fun (p, db) ->
        let color = Disco_ball.update db (Unix.gettimeofday ()) in
        Light.Point.set_color p (dim_color color))
  in
  System.make query (System.Query update)

let plugin w =
  spawn_camera w;
  spawn_cube w (Math.Vec3.v 0. 2. 0.);
  spawn_cube w (Math.Vec3.v 2. 3. 2.);
  spawn_cube w (Math.Vec3.v (-2.) 1. 2.);
  spawn_cube w (Math.Vec3.v 2. 3. (-2.));
  spawn_cube w (Math.Vec3.v (-2.) 1. (-2.));
  spawn_ground w;
  spawn_disco_ball w (Math.Vec3.v 0. 2. 5.);
  spawn_disco_ball w (Math.Vec3.v 0. 2. (-5.));
  spawn_disco_ball w (Math.Vec3.v 5. 2. 0.);
  spawn_disco_ball w (Math.Vec3.v (-5.) 2. 0.);
  spawn_disco_ball w (Math.Vec3.v 0. 5. 0.);
  World.add_system w Scheduler.Update spin;
  World.add_system w Scheduler.Update update_disco_balls

let () =
  Random.self_init ();
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
