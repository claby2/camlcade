open Camlcade
open Ecs

let _add_random_ball w =
  World.add_entity w
  |> World.with_component w
       (module Graphics.Mesh3d.C)
       (Graphics.Mesh3d.of_primitive (Graphics.Primitive.Sphere.create ()))
  |> World.with_component w
       (module Transform.C)
       Transform.(
         identity ()
         |> with_translation
              (Math.Vec3.v (Random.float 10.) (Random.float 10.)
                 (Random.float 10.))
         |> with_scale
              (Math.Vec3.v (Random.float 2.) (Random.float 2.) (Random.float 2.)))
  |> World.with_component w (module Graphics.Shader.Normal.C) ()
  |> ignore

let handle_spawn =
  let query q =
    let open Query in
    let mouse_button = q (create [ Required Input.Mouse.Button.C.id ]) in
    match Result.single mouse_button with
    | Some [ m ] -> Component.unpack (module Input.Mouse.Button.C) m
    | _ -> assert false
  in
  let spawn _world mouse_button =
    if Input.Mouse.Button.is_just_pressed mouse_button `Left then
      (* TODO: Spawn random ball*)
      print_endline "Spawn"
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
