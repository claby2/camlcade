open Tsdl
module State = State

let plugin w =
  let _state =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w (module State.Keys.C) (State.Keys.empty ())
    |> Ecs.World.with_component w
         (module State.Window_events.C)
         (State.Window_events.empty ())
    |> ignore
  in
  Ecs.World.add_system w Ecs.Scheduler.Update
    [|
      Ecs.Query.create
        [
          Ecs.Query.Required State.Keys.C.id;
          Ecs.Query.Required State.Window_events.C.id;
        ];
    |]
    (Ecs.System.Query
       (function
       | [| [ (_, [ keys; window_events ]) ] |] ->
           let keys = keys |> Ecs.Component.unpack (module State.Keys.C) in
           let window_events =
             window_events
             |> Ecs.Component.unpack (module State.Window_events.C)
           in
           State.Window_events.clear window_events;
           let event = Sdl.Event.create () in
           while Sdl.poll_event (Some event) do
             match Sdl.Event.(enum (get event typ)) with
             | `Quit -> raise Ecs.World.Quit
             | `Key_down -> State.Keys.press keys (Key.of_sdl_event event)
             | `Key_up -> State.Keys.release keys (Key.of_sdl_event event)
             | `Window_event ->
                 State.Window_events.push window_events
                   (Window.of_sdl_event event)
             | _ -> ()
           done
       | _ -> assert false))