open Camlcade
open Ecs

module Ball = struct
  type t = unit

  module C = Component.Make (struct
    type inner = t
  end)
end

let move_ball =
  System.make
    (fun q ->
      let transforms =
        q
          (Query.create ~filter:(Query.Filter.With Ball.C.id)
             [ Query.Required Transform.C.id ])
      in
      let keys = q (Query.create [ Query.Required Input.State.Keys.C.id ]) in
      ( transforms
        |> Query.Result.map (function
             | [ t ] -> Component.unpack (module Transform.C) t
             | _ -> assert false),
        keys |> Query.Result.single ))
    (System.Query
       (function
       | transforms, Some [ keys ] ->
           let keys = keys |> Component.unpack (module Input.State.Keys.C) in
           let w_pressed = Input.State.Keys.is_just_pressed keys `W in
           let s_pressed = Input.State.Keys.is_just_pressed keys `S in
           List.iter
             (fun transform ->
               let tx, ty, tz =
                 Math.Vec3.to_tuple (Transform.translation transform)
               in
               if w_pressed then
                 Transform.set_translation transform
                   (Math.Vec3.v tx ty (tz +. 0.001));
               if s_pressed then
                 Transform.set_translation transform
                   (Math.Vec3.v tx ty (tz -. 0.001)))
             transforms
       | _ -> assert false))

let plugin w =
  let _camera =
    World.add_entity w
    |> World.with_component w
         (module Graphics.Camera.Dim3.C)
         (Graphics.Camera.Dim3.create ~pos:(Math.Vec3.v 3. 3. 3.) ())
  in
  let add_ball w =
    World.add_entity w
    |> World.with_component w
         (module Graphics.Mesh3d.C)
         (Graphics.Mesh3d.of_primitive
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
