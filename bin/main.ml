open Camlcade

module Ball = struct
  type t = unit

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end

let move_ball =
  Ecs.System.make
    (fun querier ->
      let transforms =
        querier
          (Ecs.Query.create ~filter:(Ecs.Query.Filter.With Ball.C.id)
             [ Ecs.Query.Required Transform.C.id ])
      in
      let keys =
        querier (Ecs.Query.create [ Ecs.Query.Required Input.State.Keys.C.id ])
      in
      (transforms, keys |> Ecs.Query.Result.single))
    (Ecs.System.Query
       (function
       | transforms, Some [ keys ] ->
           let keys =
             keys |> Ecs.Component.unpack (module Input.State.Keys.C)
           in
           let w_pressed = Input.State.Keys.is_just_pressed keys `W in
           let s_pressed = Input.State.Keys.is_just_pressed keys `S in
           Ecs.Query.Result.iter transforms (function
             | [ transform ] ->
                 let transform =
                   transform |> Ecs.Component.unpack (module Transform.C)
                 in
                 let tx, ty, tz =
                   Math.Vec3.to_tuple (Transform.translation transform)
                 in
                 if w_pressed then
                   Transform.set_translation transform
                     (Math.Vec3.v tx ty (tz +. 0.001));
                 if s_pressed then
                   Transform.set_translation transform
                     (Math.Vec3.v tx ty (tz -. 0.001))
             | _ -> assert false)
       | _ -> assert false))

let plugin w =
  let _camera =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w
         (module Graphics.Camera.Dim3.C)
         (Graphics.Camera.Dim3.create ~pos:(Math.Vec3.v 3. 3. 3.) ())
  in
  let add_ball w =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w
         (module Graphics.Mesh3d.C)
         (Graphics.Mesh3d.of_primitive
            (Graphics.Primitive.Sphere.create ~param1:10 ~param2:10 ()))
    |> Ecs.World.with_component w (module Transform.C) (Transform.identity ())
    |> Ecs.World.with_component w
         (module Graphics.Shader.C)
         Graphics.Shader.phong
    |> Ecs.World.with_component w (module Ball.C) ()
  in
  add_ball w |> ignore;

  Ecs.World.add_system w Ecs.Scheduler.Update move_ball

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
