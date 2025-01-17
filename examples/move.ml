(** Move a ball up and down with the W and S keys. *)

open Camlcade
open Ecs

module Ball = struct
  type t = unit

  module C = Component.Make (struct
    type inner = t
  end)
end

let move_ball =
  let query w =
    let transforms =
      World.query ~filter:(Query.Filter.With Ball.C.id) w
        Query.(Req (module Transform.C) @ Nil)
      |> List.map (fun (_, (t, ())) -> t)
    in
    let _, (k, ()) =
      World.query w Query.(Req (module Input.Keyboard.C) @ Nil) |> List.hd
    in
    (transforms, k)
  in
  let move (transforms, keyboard) =
    let w_pressed = Input.Keyboard.is_pressed keyboard `W in
    let s_pressed = Input.Keyboard.is_pressed keyboard `S in
    List.iter
      (fun transform ->
        let tx, ty, tz = Math.Vec3.to_tuple (Transform.translation transform) in
        if w_pressed then
          Transform.set_translation transform (Math.Vec3.v tx ty (tz -. 0.001));
        if s_pressed then
          Transform.set_translation transform (Math.Vec3.v tx ty (tz +. 0.001)))
      transforms
  in
  System.make query (System.Query move)

let plugin w =
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
           |> with_translation (Math.Vec3.v 3. 3. 3.)
           |> with_look_at (Math.Vec3.v 0. 0. 0.))
  in
  let add_ball w =
    World.add_entity w
    |> World.with_component w
         (module Graphics.Mesh3d.C)
         (Graphics.Primitive.to_mesh3d
            (Graphics.Primitive.Sphere.create ~param1:10 ~param2:10 ()))
    |> World.with_component w (module Transform.C) (Transform.identity ())
    |> World.with_component w (module Graphics.Shader.Normal.C) ()
    |> World.with_component w (module Ball.C) ()
  in
  add_ball w |> ignore;

  World.add_system w Scheduler.Update move_ball

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
