open Tsdl

module Key_event = Ecs.Event.Make (struct
  type t = Key.t
end)

module Keyboard = Button_state.Make (Key) (Key_event)

module Window_event = Ecs.Event.Make (struct
  type t = Sdl.Event.window_event_enum
end)

module Mouse = struct
  module Button_event = Ecs.Event.Make (struct
    type t = Mouse_button.t
  end)

  module Button = Button_state.Make (Mouse_button) (Button_event)

  type motion = { x : int; y : int; dx : int; dy : int }

  let x m = m.x
  let y m = m.y
  let dx m = m.dx
  let dy m = m.dy

  module Motion_event = Ecs.Event.Make (struct
    type t = motion
  end)
end

let write_events =
  let query q =
    let open Ecs.Query in
    let key = q (create [ Required Key_event.C.id ]) in
    let window = q (create [ Required Window_event.C.id ]) in
    let mouse_button = q (create [ Required Mouse.Button_event.C.id ]) in
    let mouse_motion = q (create [ Required Mouse.Motion_event.C.id ]) in
    ( Result.single key,
      Result.single window,
      Result.single mouse_button,
      Result.single mouse_motion )
  in
  let write key window mouse_button mouse_motion =
    let event = Sdl.Event.create () in

    (* Convenent functions for extracting data from SDL events. *)
    let scan_key e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
    let scan_window e = Sdl.Event.(window_event_enum (get e window_event_id)) in
    let scan_mouse_button e =
      Mouse_button.of_int Sdl.Event.(get e mouse_button_button)
    in

    (* Handle SDL events. *)
    while Sdl.poll_event (Some event) do
      match Sdl.Event.(enum (get event typ)) with
      | `Quit -> raise Ecs.World.Quit
      | `Key_down -> Key_event.write key (Key.Down (scan_key event))
      | `Key_up -> Key_event.write key (Key.Up (scan_key event))
      | `Window_event -> Window_event.write window (scan_window event)
      | `Mouse_button_down ->
          Mouse.Button_event.write mouse_button
            (Mouse_button.Down (scan_mouse_button event))
      | `Mouse_button_up ->
          Mouse.Button_event.write mouse_button
            (Mouse_button.Up (scan_mouse_button event))
      | `Mouse_motion ->
          let x = Sdl.Event.(get event mouse_motion_x) in
          let y = Sdl.Event.(get event mouse_motion_y) in
          let dx = Sdl.Event.(get event mouse_motion_xrel) in
          let dy = Sdl.Event.(get event mouse_motion_yrel) in
          Mouse.Motion_event.write mouse_motion { x; y; dx; dy }
      | _ -> ()
    done
  in
  Ecs.System.make query
    (Ecs.System.Query
       (function
       | Some [ k ], Some [ w ], Some [ mb ], Some [ mm ] ->
           let k = k |> Ecs.Component.unpack (module Key_event.C) in
           let w = w |> Ecs.Component.unpack (module Window_event.C) in
           let mb = mb |> Ecs.Component.unpack (module Mouse.Button_event.C) in
           let mm = mm |> Ecs.Component.unpack (module Mouse.Motion_event.C) in
           write k w mb mm
       | _ -> assert false))

let plugin w =
  let _state =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w
         (module Window_event.C)
         (Window_event.empty ())
    |> Ecs.World.with_component w (module Key_event.C) (Key_event.empty ())
    |> Ecs.World.with_component w (module Keyboard.C) (Keyboard.create ())
    |> Ecs.World.with_component w
         (module Mouse.Button_event.C)
         (Mouse.Button_event.empty ())
    |> Ecs.World.with_component w
         (module Mouse.Button.C)
         (Mouse.Button.create ())
    |> Ecs.World.with_component w
         (module Mouse.Motion_event.C)
         (Mouse.Motion_event.empty ())
    |> ignore
  in

  (* The ordering of systems is important. It should be clear -> write -> update. *)

  (* Clear events systems. *)
  Ecs.World.add_system w Ecs.Scheduler.Update Key_event.clear_system;
  Ecs.World.add_system w Ecs.Scheduler.Update Window_event.clear_system;
  Ecs.World.add_system w Ecs.Scheduler.Update Mouse.Button_event.clear_system;
  Ecs.World.add_system w Ecs.Scheduler.Update Mouse.Motion_event.clear_system;

  (* Write. *)
  Ecs.World.add_system w Ecs.Scheduler.Update write_events;

  (* Button state update systems. *)
  Ecs.World.add_system w Ecs.Scheduler.Update Keyboard.update_system;
  Ecs.World.add_system w Ecs.Scheduler.Update Mouse.Button.update_system
