(** Spawn random balls on left click. *)

open Camlcade
open Ecs

let add_random_ball w =
  World.add_entity w
  |> World.with_component w
       (module Graphics.Mesh3d.C)
       (Graphics.Mesh3d.of_primitive
          (Graphics.Primitive.Sphere.create ~param1:20 ~param2:20 ()))
  |> World.with_component w
       (module Transform.C)
       Transform.(
         identity ()
         |> with_translation
              (Math.Vec3.v
                 (Random.float 10. -. 5.)
                 (Random.float 10. -. 5.)
                 (Random.float 10. -. 5.))
         |> with_scale
              (Math.Vec3.v (Random.float 2.) (Random.float 2.) (Random.float 2.)))
  |> World.with_component w (module Graphics.Shader.Normal.C) ()
  |> ignore

let handle_spawn =
  let query w =
    let _, (mb, ()) =
      World.query w Query.(Req (module Input.Mouse.Button.C) @ Nil) |> List.hd
    in
    mb
  in
  let spawn world mouse_button =
    if Input.Mouse.Button.is_just_pressed mouse_button `Left then (
      print_endline
        (Printf.sprintf "%f: Spawned a random ball!" (Unix.gettimeofday ()));
      add_random_ball world)
  in
  System.make query (System.Immediate spawn)

let add_camera w =
  World.add_entity w
  |> World.with_component w (module Graphics.Camera3d.C) ()
  |> World.with_component w
       (module Graphics.Camera.Projection.C)
       (Graphics.Camera.Projection.perspective ())
  |> World.with_component w
       (module Transform.C)
       Transform.(
         identity ()
         |> with_translation (Math.Vec3.v 0. 7. 14.)
         |> with_look_at (Math.Vec3.v 0. 0. 0.))

let plugin w =
  add_camera w |> ignore;

  (* TODO: Use more interesting primitives *)
  World.add_system w Scheduler.Update handle_spawn

let () =
  Random.self_init ();
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
