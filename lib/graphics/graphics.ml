let initialize_window ~gl = function
  | [| [ (_, [ context ]) ] |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.initialize ~gl context
  | _ -> assert false

let destroy_window = function
  | [| [ (_, [ context ]) ] |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.destroy context
  | _ -> assert false

let plugin w =
  Ecs.World.add_entity w
  |> Ecs.World.with_component w
       (Ecs.Component.pack (module Context.C) (Context.T.empty ()))
  |> ignore;

  Ecs.World.add_system w Ecs.Scheduler.Startup
    [| Ecs.Query.create [ Ecs.Query.Required Context.C.id ] |]
    (Ecs.System.Query (initialize_window ~gl:(4, 0)));

  Ecs.World.add_system w Ecs.Scheduler.Last
    [| Ecs.Query.create [ Ecs.Query.Required Context.C.id ] |]
    (Ecs.System.Query destroy_window)
