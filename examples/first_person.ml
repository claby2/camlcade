open Camlcade
open Ecs

module FirstPersonCamera = struct
  type t = unit

  module C = Component.Make (struct
    type inner = t
  end)
end

let calculate_move transform w a s d =
  let move = ref (Math.Vec3.v 0. 0. 0.) in
  let forward = Transform.forward transform in
  let up = Transform.local_y transform in
  if w then move := Math.Vec3.add !move forward;
  (if a then move := Math.Vec3.(sub !move (normalize (cross forward up))));
  if s then move := Math.Vec3.sub !move forward;
  (if d then move := Math.Vec3.(add !move (normalize (cross forward up))));
  Math.Vec3.normalize !move

let handle_keyboard =
  let query q =
    let open Query in
    let transform =
      q
        (create ~filter:(Filter.With FirstPersonCamera.C.id)
           [ Required Transform.C.id ])
    in
    let keyboard = q (create [ Required Input.Keyboard.C.id ]) in
    ( (match Result.single transform with
      | Some [ t ] -> Component.unpack (module Transform.C) t
      | _ -> assert false),
      match Result.single keyboard with
      | Some [ k ] -> Component.unpack (module Input.Keyboard.C) k
      | _ -> assert false )
  in
  let factor = 0.001 in
  let move (transform, keyboard) =
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
  World.add_system w Scheduler.Update handle_keyboard;
  (* TODO: Handle mouse *)
  ()

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
