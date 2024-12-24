open Camlcade

module Ball = struct
  type t = unit

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end

let move_ball =
  Ecs.System.Query
    (function
    | [| [ (_, [ transform ]) ] |] ->
        let transform =
          transform |> Ecs.Component.unpack (module Transform.C)
        in
        let tx, _, tz = Math.Vec3.to_tuple (Transform.translation transform) in
        let ty = sin (Unix.gettimeofday ()) in
        Transform.set_translation transform (Math.Vec3.v tx ty tz)
    | _ -> assert false)

let plugin w =
  let _camera =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w
         (module Graphics.Camera.Dim3.C)
         (Graphics.Camera.Dim3.create ~pos:(Math.Vec3.v 3. 3. 3.) ())
  in
  let _sphere =
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

  Ecs.World.add_system w Ecs.Scheduler.Update
    [|
      Ecs.Query.create ~filter:(Ecs.Query.Filter.With Ball.C.id)
        [ Ecs.Query.Required Transform.C.id ];
    |]
    move_ball

let () =
  let app =
    App.create () |> App.add_plugin Graphics.plugin |> App.add_plugin plugin
  in

  App.run app
