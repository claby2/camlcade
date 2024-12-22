open Camlcade

let () =
  let app =
    App.create ()
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin (fun w ->
           let _camera =
             Ecs.World.add_entity w
             |> Ecs.World.with_component w
                  (Ecs.Component.pack
                     (module Graphics.Camera.Dim3.C)
                     (Graphics.Camera.Dim3.T.create
                        ~pos:(Math.Vec3.make 10. 0. 0.) ()))
           in
           let _sphere =
             Ecs.World.add_entity w
             |> Ecs.World.with_component w
                  (Ecs.Component.pack
                     (module Graphics.Mesh3d.C)
                     (Graphics.Mesh3d.T.from_mesh (Graphics.Mesh.sphere ())))
             |> Ecs.World.with_component w
                  (Ecs.Component.pack
                     (module Graphics.Shader.C)
                     Graphics.Shader.T.phong)
           in

           ())
  in

  App.run app
