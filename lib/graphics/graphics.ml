open Util
open Tgl4
open Tsdl
module Context = Context
module Camera = Camera
module Camera3d = Camera.Camera3d
module Mesh3d = Mesh3d
module Shader = Shader
module Vertex_mesh = Vertex_mesh
module Primitive = Primitive

let initialize ~gl =
  Ecs.System.make
    (fun w ->
      let open Ecs in
      let _, (c, ()) =
        World.query w Query.(Req (module Context.C) @ Nil) |> List.hd
      in
      c)
    (Ecs.System.Query
       (fun context ->
         Context.initialize ~gl context;
         (* Initialize shaders *)
         Shader.initialize Shader.normal))

let render =
  Ecs.System.make
    (fun w ->
      let open Ecs in
      let _, (c, ()) =
        World.query w Query.(Req (module Context.C) @ Nil) |> List.hd
      in
      c)
    (Ecs.System.Query
       (fun context ->
         Context.render context;
         Gl.clear_color 0. 0. 0. 1.;
         Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
         check_gl_error ()))

let handle_events =
  Ecs.System.make
    (fun w ->
      let open Ecs in
      let c = World.query w Query.(Req (module Context.C) @ Nil) in
      let we = World.query w Query.(Req (module Input.Window_event.C) @ Nil) in
      ( List.nth_opt c 0 |> Option.map (fun (_, (c, ())) -> c),
        List.nth_opt we 0 |> Option.map (fun (_, (we, ())) -> we) ))
    (Ecs.System.Query
       (function
       | Some context, Some window_event ->
           List.iter
             (function
               | `Exposed | `Resized ->
                   let w, h = Context.get_window_size context in
                   Gl.viewport 0 0 w h
               | _ -> ())
             (Input.Window_event.read window_event)
       | _ ->
           let _key_scancode e =
             Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode)
           in
           let _window_event e =
             Sdl.Event.(window_event_enum (get e window_event_id))
           in
           let event = Sdl.Event.create () in
           while Sdl.poll_event (Some event) do
             match Sdl.Event.(enum (get event typ)) with
             | `Quit -> raise Ecs.World.Quit
             | _ -> ()
           done))

let cleanup =
  Ecs.System.make
    (fun w ->
      let open Ecs in
      let c_entity, (c, ()) =
        World.query w Query.(Req (module Context.C) @ Nil) |> List.hd
      in
      let m3d = World.query w Query.(Req (module Mesh3d.C) @ Nil) in
      (c_entity, c, List.map (fun (_, (m3d, ())) -> m3d) m3d))
    (Ecs.System.Immediate
       (fun w ->
         fun (context_entity, context, meshes3d) ->
          Context.destroy context;
          Ecs.World.remove_entity w context_entity;
          (* Destroy shaders *)
          Shader.destroy Shader.normal;
          (* Destroy meshes *)
          List.iter Mesh3d.destroy meshes3d))

let plugin w =
  let add_context w =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w (module Context.C) (Context.empty ())
    |> ignore
  in
  add_context w;

  Ecs.World.add_system w Ecs.Scheduler.Startup (initialize ~gl:(4, 0));

  Ecs.World.add_system w Ecs.Scheduler.Update render;
  Ecs.World.add_system w Ecs.Scheduler.Update handle_events;
  Ecs.World.add_system w Ecs.Scheduler.Update Shader.shade_normal;

  Ecs.World.add_system w Ecs.Scheduler.Last cleanup
