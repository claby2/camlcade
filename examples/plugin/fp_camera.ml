open Ecs

module C = Component.Make (struct
  type inner = unit
end)

let handle_keyboard factor =
  let query w =
    let transforms =
      World.query ~filter:(Query.Filter.With C.id) w
        Query.[Req (module Transform.C)]
      |> List.map (fun (_, (t, ())) -> t)
    in
    let k =
      World.query w Query.[Req (module Input.Keyboard.C)]
      |> List.map (fun (_, (k, ())) -> k)
    in
    (transforms, match k with k :: _ -> Some k | _ -> None)
  in
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
  let move (transforms, keyboard) =
    match keyboard with
    | Some keyboard ->
        let is_pressed = Input.Keyboard.is_pressed keyboard in
        let w = is_pressed `W in
        let a = is_pressed `A in
        let s = is_pressed `S in
        let d = is_pressed `D in
        let space = is_pressed `Space in
        let shift = is_pressed `Lshift in

        transforms
        |> List.iter (fun transform ->
               (* Handle WASD movement *)
               let delta = calculate_move transform w a s d in
               if Math.Vec3.norm delta > 0. then
                 Transform.set_translation transform
                   Math.Vec3.(
                     add (Transform.translation transform) (smul factor delta));

               (* Handle space and shift movement *)
               let x, y, z =
                 Math.Vec3.to_tuple (Transform.translation transform)
               in
               if space then
                 Transform.set_translation transform
                   (Math.Vec3.v x (y +. factor) z);
               if shift then
                 Transform.set_translation transform
                   (Math.Vec3.v x (y -. factor) z))
    | None -> ()
  in
  System.make query (System.Query move)

let handle_mouse sensitivity =
  let query w =
    let transforms =
      World.query ~filter:(Query.Filter.With C.id) w
        Query.[Req (module Transform.C)]
      |> List.map (fun (_, (t, ())) -> t)
    in
    let mm =
      World.query w Query.[Req (module Input.Mouse.Motion_event.C)]
      |> List.map (fun (_, (mm, ())) -> mm)
    in
    (transforms, match mm with mm :: _ -> Some mm | _ -> None)
  in
  let update_rotation motion transform =
    let yaw_axis = Math.Vec3.oy in
    let pitch_axis = Math.Vec3.ox in
    let dx = float_of_int (Input.Mouse.dx motion) in
    let dy = float_of_int (Input.Mouse.dy motion) in
    let yaw = -.dx *. sensitivity in
    let pitch = -.dy *. sensitivity in
    let new_rotation =
      Math.Quat.(
        mul (rot3_axis yaw_axis yaw)
          (mul (Transform.rotation transform) (rot3_axis pitch_axis pitch)))
    in
    Transform.set_rotation transform new_rotation
  in
  let move (transforms, mouse_motion) =
    match mouse_motion with
    | Some mouse_motion ->
        Input.Mouse.Motion_event.read mouse_motion
        |> List.iter (fun motion ->
               transforms |> List.iter (update_rotation motion))
    | None -> ()
  in
  System.make query (System.Query move)

let set_fullscreen =
  System.make
    (fun w ->
      World.query w Query.[Req (module Graphics.Context.C)]
      |> List.map (fun (_, (c, ())) -> c))
    (System.Query
       (List.iter (fun context ->
            Graphics.Context.set_window_fullscreen context
              Graphics.Context.Window.fullscreen;
            Graphics.Context.set_relative_mouse_mode true)))

let plugin ?(mouse_sensitivity = 0.001) ?(move_factor = 0.001)
    ?(fullscreen = false) w =
  if fullscreen then World.add_system w Scheduler.Startup set_fullscreen;
  World.add_system w Scheduler.Update (handle_keyboard move_factor);
  World.add_system w Scheduler.Update (handle_mouse mouse_sensitivity)
