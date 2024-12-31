open Tsdl
module Key_event = Key_event
module Keyboard = Keyboard

module Window_event = Ecs.Event.Make (struct
  type t = Window.t
end)

let write_events =
  let query q =
    let open Ecs.Query in
    let key = q (create [ Required Key_event.C.id ]) in
    let window = q (create [ Required Window_event.C.id ]) in
    (Result.single key, Result.single window)
  in
  let write key window =
    let event = Sdl.Event.create () in
    while Sdl.poll_event (Some event) do
      match Sdl.Event.(enum (get event typ)) with
      | `Quit -> raise Ecs.World.Quit
      | `Key_down -> Key_event.write key (Key.Down (Key.scan event))
      | `Key_up -> Key_event.write key (Key.Up (Key.scan event))
      | `Window_event -> Window_event.write window (Window.scan event)
      | _ -> ()
    done
  in
  Ecs.System.make query
    (Ecs.System.Query
       (function
       | Some [ key ], Some [ window ] ->
           let key = key |> Ecs.Component.unpack (module Key_event.C) in
           let window =
             window |> Ecs.Component.unpack (module Window_event.C)
           in
           write key window
       | _ -> assert false))

let plugin w =
  let _state =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w (module Key_event.C) (Key_event.empty ())
    |> Ecs.World.with_component w
         (module Window_event.C)
         (Window_event.empty ())
    |> Ecs.World.with_component w (module Keyboard.C) (Keyboard.create ())
    |> ignore
  in

  Ecs.World.add_system w Ecs.Scheduler.Update Key_event.clear_system;
  Ecs.World.add_system w Ecs.Scheduler.Update Window_event.clear_system;
  Ecs.World.add_system w Ecs.Scheduler.Update write_events;
  Ecs.World.add_system w Ecs.Scheduler.Update Keyboard.update_system
