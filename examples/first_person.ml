open Camlcade
open Ecs

module FirstPersonCamera = struct
  type t = unit

  module C = Component.Make (struct
    type inner = t
  end)
end

let setup_window =
  System.make
    (fun w ->
      let _, (c, ()) =
        World.query w Query.(Req (module Graphics.Context.C) @ Nil) |> List.hd
      in
      c)
    (System.Query
       (fun context ->
         Graphics.Context.set_window_fullscreen context
           Graphics.Context.Window.fullscreen;
         Graphics.Context.set_relative_mouse_mode true))

let handle_keyboard =
  let calculate_move transform w a s d =
    let move = ref (Math.Vec3.v 0. 0. 0.) in
    let forward = Transform.forward transform in
    let up = Transform.local_y transform in
    if w then move := Math.Vec3.add !move forward;
    (if a then move := Math.Vec3.(sub !move (normalize (cross forward up))));
    if s then move := Math.Vec3.sub !move forward;
    (if d then move := Math.Vec3.(add !move (normalize (cross forward up))));
    Math.Vec3.normalize !move
  in
  let query w =
    let _, (t, ()) =
      World.query ~filter:(Query.Filter.With FirstPersonCamera.C.id) w
        Query.(Req (module Transform.C) @ Nil)
      |> List.hd
    in
    let _, (k, ()) =
      World.query w Query.(Req (module Input.Keyboard.C) @ Nil) |> List.hd
    in
    (t, k)
  in
  let move (transform, keyboard) =
    let factor = 0.001 in
    let is_pressed = Input.Keyboard.is_pressed keyboard in
    let w = is_pressed `W in
    let a = is_pressed `A in
    let s = is_pressed `S in
    let d = is_pressed `D in

    (* Handle WASD movement *)
    let move = calculate_move transform w a s d in
    if Math.Vec3.norm move > 0. then
      Transform.set_translation transform
        Math.Vec3.(add (Transform.translation transform) (smul factor move));

    let space = is_pressed `Space in
    let shift = is_pressed `Lshift in

    (* Handle space and shift movement *)
    let x, y, z = Math.Vec3.to_tuple (Transform.translation transform) in
    if space then
      Transform.set_translation transform (Math.Vec3.v x (y +. factor) z);
    if shift then
      Transform.set_translation transform (Math.Vec3.v x (y -. factor) z)
  in
  System.make query (System.Query move)

let handle_mouse =
  let query w =
    let _, (t, ()) =
      World.query ~filter:(Query.Filter.With FirstPersonCamera.C.id) w
        Query.(Req (module Transform.C) @ Nil)
      |> List.hd
    in
    let _, (mm, ()) =
      World.query w Query.(Req (module Input.Mouse.Motion_event.C) @ Nil)
      |> List.hd
    in
    (t, mm)
  in
  let move (transform, mouse_motion) =
    let factor = 0.001 in
    let yaw_axis = Math.Vec3.oy in
    let pitch_axis = Math.Vec3.ox in
    List.iter
      (fun motion ->
        let dx = float_of_int (Input.Mouse.dx motion) in
        let dy = float_of_int (Input.Mouse.dy motion) in
        let yaw = -.dx *. factor in
        let pitch = -.dy *. factor in
        let new_rotation =
          Math.Quat.(
            mul (rot3_axis yaw_axis yaw)
              (mul (Transform.rotation transform) (rot3_axis pitch_axis pitch)))
        in
        Transform.set_rotation transform new_rotation)
      (Input.Mouse.Motion_event.read mouse_motion)
  in
  System.make query (System.Query move)

let plugin w =
  let _cuboid =
    World.add_entity w
    |> World.with_component w
         (module Graphics.Mesh3d.C)
         (Graphics.Mesh3d.of_primitive (Graphics.Primitive.Cuboid.create ()))
    |> World.with_component w (module Graphics.Shader.Normal.C) ()
  in

  let _camera =
    World.add_entity w
    |> World.with_component w (module Graphics.Camera3d.C) ()
    |> World.with_component w
         (module Graphics.Camera.Projection.C)
         (Graphics.Camera.Projection.perspective ())
    |> World.with_component w (module Transform.C) (Transform.identity ())
    |> World.with_component w (module FirstPersonCamera.C) ()
  in

  World.add_system w Scheduler.Startup setup_window;
  World.add_system w Scheduler.Update handle_keyboard;
  World.add_system w Scheduler.Update handle_mouse

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
