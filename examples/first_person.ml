(** First person camera example.

    The first-person camera logic is implemented by the [Plugin.Fp_camera]
    plugin. *)

open Plugin
open Camlcade
open Ecs

let plugin w =
  let _cuboid =
    World.add_entity w
    |> World.with_component w
         (module Graphics.Mesh3d.C)
         (Graphics.Primitive.to_mesh3d (Graphics.Primitive.Cuboid.create ()))
    |> World.with_component w (module Graphics.Shader.Normal.C) ()
  in

  let _camera =
    World.add_entity w
    |> World.with_component w (module Graphics.Camera3d.C) ()
    |> World.with_component w
         (module Graphics.Camera.Projection.C)
         (Graphics.Camera.Projection.perspective ())
    |> World.with_component w (module Transform.C) (Transform.identity ())
    |> World.with_component w (module Fp_camera.C) ()
  in
  ()

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin (Fp_camera.plugin ~fullscreen:true)
    |> App.add_plugin plugin
  in

  App.run app
