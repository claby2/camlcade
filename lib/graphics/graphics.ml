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
    (fun q ->
      let open Ecs.Query in
      let c = q (create [ Required Context.C.id ]) in
      c |> Result.as_single (module Context.C) |> Option.get)
    (Ecs.System.Query
       (fun context ->
         Context.initialize ~gl context;
         (* Initialize shaders *)
         Shader.initialize Shader.normal))

let render =
  Ecs.System.make
    (fun q ->
      let open Ecs.Query in
      let c = q (create [ Required Context.C.id ]) in
      c |> Result.as_single (module Context.C) |> Option.get)
    (Ecs.System.Query
       (fun context ->
         Context.render context;
         Gl.clear_color 0. 0. 0. 1.;
         Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
         check_gl_error ()))

let handle_events =
  Ecs.System.make
    (fun q ->
      let open Ecs.Query in
      let c = q (create [ Required Context.C.id ]) in
      let we = q (create [ Required Input.Window_event.C.id ]) in
      ( c |> Result.as_single (module Context.C),
        we |> Result.as_single (module Input.Window_event.C) ))
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
    (fun q ->
      let open Ecs.Query in
      let c = q (create [ Required Context.C.id ]) in
      let m3d = q (create [ Required Mesh3d.C.id ]) in
      ( c |> Ecs.Query.Result.entity_single,
        m3d |> Ecs.Query.Result.as_list (module Mesh3d.C) ))
    (Ecs.System.Immediate
       (fun w -> function
         | Some (context_entity, [ context ]), meshes3d ->
             (* Cleanup context *)
             let context = context |> Ecs.Component.unpack (module Context.C) in
             Context.destroy context;
             Ecs.World.remove_entity w context_entity;
             (* Destroy shaders *)
             Shader.destroy Shader.normal;
             (* Destroy meshes *)
             List.iter Mesh3d.destroy meshes3d
         | _ -> assert false))

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
