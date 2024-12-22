let initialize ~gl = function
  | [| [ (_, [ context ]) ]; meshes3d |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.initialize ~gl context;

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
  | [| [ (context_entity, [ context ]) ]; meshes3d |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.destroy context;
      Ecs.World.remove_entity w context_entity;

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
  Ecs.World.add_entity w
  |> Ecs.World.with_component w
       (Ecs.Component.pack (module Context.C) (Context.T.empty ()))
  |> ignore;

  Ecs.World.add_system w Ecs.Scheduler.Startup
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    (Ecs.System.Query (initialize ~gl:(4, 0)));

  Ecs.World.add_system w Ecs.Scheduler.Update
    [| Ecs.Query.create [ Ecs.Query.Required Context.C.id ] |]
    (Ecs.System.Query render);

  Ecs.World.add_system w Ecs.Scheduler.Last
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    (Ecs.System.Immediate cleanup)
