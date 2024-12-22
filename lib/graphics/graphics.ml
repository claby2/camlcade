module Camera = Camera
module Mesh = Mesh
module Mesh3d = Mesh3d

let initialize ~gl = function
  | [| [ (_, [ context ]) ]; [ (_, [ shader_manager ]) ]; meshes3d |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.initialize ~gl context;

      let shader_manager =
        shader_manager |> Ecs.Component.unpack |> Shader.Manager.C.of_base
      in
      Shader.Manager.T.initialize shader_manager;

      meshes3d
      |> List.iter (fun (_, components) ->
             match components with
             | [ mesh3d ] ->
                 let mesh3d =
                   mesh3d |> Ecs.Component.unpack |> Mesh3d.C.of_base
                 in
                 Mesh3d.T.initialize mesh3d
             | _ -> assert false)
  | _ -> assert false

let render = function
  | [| [ (_, [ context ]) ] |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.render context
  | _ -> assert false

let cleanup w = function
  | [|
      [ (context_entity, [ context ]) ];
      [ (shader_manager_entity, [ shader_manager ]) ];
      meshes3d;
    |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.destroy context;
      Ecs.World.remove_entity w context_entity;

      let shader_manager =
        shader_manager |> Ecs.Component.unpack |> Shader.Manager.C.of_base
      in
      Shader.Manager.T.destroy shader_manager;
      Ecs.World.remove_entity w shader_manager_entity;

      meshes3d
      |> List.iter (fun (_, components) ->
             match components with
             | [ mesh3d ] ->
                 let mesh3d =
                   mesh3d |> Ecs.Component.unpack |> Mesh3d.C.of_base
                 in
                 Mesh3d.T.destroy mesh3d
             | _ -> assert false)
  | _ -> assert false

let plugin w =
  let add_context w =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w
         (Ecs.Component.pack (module Context.C) (Context.T.empty ()))
    |> ignore
  in
  let add_shader_manager w =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w
         (Ecs.Component.pack
            (module Shader.Manager.C)
            (Shader.Manager.T.empty ()))
    |> ignore
  in
  add_context w;
  add_shader_manager w;

  Ecs.World.add_system w Ecs.Scheduler.Startup
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Shader.Manager.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    (Ecs.System.Query (initialize ~gl:(4, 0)));

  Ecs.World.add_system w Ecs.Scheduler.Update
    [| Ecs.Query.create [ Ecs.Query.Required Context.C.id ] |]
    (Ecs.System.Query render);

  Ecs.World.add_system w Ecs.Scheduler.Last
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Shader.Manager.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    (Ecs.System.Immediate cleanup)
