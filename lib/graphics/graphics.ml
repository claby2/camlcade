open Util
open Tgl4
open Tsdl
module Camera = Camera
module Mesh3d = Mesh3d
module Shader = Shader
module Vertex_mesh = Vertex_mesh
module Primitive = Primitive

let initialize ~gl =
  Ecs.System.make
    (fun q ->
      let context = q (Ecs.Query.create [ Ecs.Query.Required Context.C.id ]) in
      let shaders = q (Ecs.Query.create [ Ecs.Query.Required Shader.C.id ]) in
      let meshes3d = q (Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ]) in
      ( context |> Ecs.Query.Result.single,
        shaders
        |> Ecs.Query.Result.map (function
             | [ s ] -> Ecs.Component.unpack (module Shader.C) s
             | _ -> assert false),
        meshes3d
        |> Ecs.Query.Result.map (function
             | [ m ] -> Ecs.Component.unpack (module Mesh3d.C) m
             | _ -> assert false) ))
    (Ecs.System.Query
       (function
       | Some [ context ], shaders, meshes3d ->
           let context = context |> Ecs.Component.unpack (module Context.C) in
           Context.initialize ~gl context;
           (* Initialize shaders *)
           shaders |> List.iter Shader.initialize;
           (* Initialize and install 3d meshes *)
           meshes3d
           |> List.iter (fun mesh3d ->
                  (* TODO: Is this the right place to install VBOs? *)
                  Mesh3d.initialize mesh3d;
                  Mesh3d.install_vbo mesh3d)
       | _ -> assert false))

let render =
  Ecs.System.make
    (fun q ->
      let context = q (Ecs.Query.create [ Ecs.Query.Required Context.C.id ]) in
      context |> Ecs.Query.Result.single)
    (Ecs.System.Query
       (function
       | Some [ context ] ->
           let context = context |> Ecs.Component.unpack (module Context.C) in
           Context.render context
       | _ -> assert false))

let handle_events =
  Ecs.System.make
    (fun q ->
      let context = q (Ecs.Query.create [ Ecs.Query.Required Context.C.id ]) in
      let window_events =
        q
          (Ecs.Query.create
             [ Ecs.Query.Required Input.State.Window_events.C.id ])
      in
      ( context |> Ecs.Query.Result.single,
        window_events |> Ecs.Query.Result.single ))
    (Ecs.System.Query
       (function
       | Some [ context ], Some [ window_events ] ->
           let context = context |> Ecs.Component.unpack (module Context.C) in
           let window_events =
             window_events
             |> Ecs.Component.unpack (module Input.State.Window_events.C)
           in
           Input.State.Window_events.iter window_events (function
             | `Exposed | `Resized ->
                 let w, h = Context.get_window_size context in
                 Gl.viewport 0 0 w h
             | _ -> ())
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

let shade3d =
  Ecs.System.make
    (fun q ->
      let cameras =
        q (Ecs.Query.create [ Ecs.Query.Required Camera.Dim3.C.id ])
      in
      let entities =
        q
          (Ecs.Query.create
             [
               Ecs.Query.Required Mesh3d.C.id;
               Ecs.Query.Required Shader.C.id;
               Ecs.Query.Optional Transform.C.id;
             ])
      in
      ( cameras
        |> Ecs.Query.Result.map (function
             | [ c ] -> Ecs.Component.unpack (module Camera.Dim3.C) c
             | _ -> assert false),
        entities
        |> Ecs.Query.Result.map (function
             | [ m; s; t ] ->
                 ( Ecs.Component.unpack (module Mesh3d.C) m,
                   Ecs.Component.unpack (module Shader.C) s,
                   Ecs.Component.unpack_opt (module Transform.C) t )
             | _ -> assert false) ))
    (Ecs.System.Query
       (fun (cameras, entities) ->
         check_gl_error ();
         Gl.clear_color 0. 0. 0. 1.;
         Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
         let render_entity c mesh3d shader t_opt =
           match Shader.tag_opt shader with
           | Some Shader.Phong ->
               Shader.with_shader shader (fun pid ->
                   load_matrix4fv (Camera.Dim3.view c) pid "viewMatrix";
                   load_matrix4fv (Camera.Dim3.projection c) pid
                     "projectionMatrix";

                   let transform =
                     match t_opt with
                     | Some t -> Transform.compute_matrix t
                     | None -> Math.Mat4.id
                   in

                   let normal_matrix =
                     Math.Mat3.inv
                       (Math.Mat3.transpose (Math.Mat3.of_m4 transform))
                   in
                   load_matrix4fv transform pid "modelMatrix";
                   load_matrix3fv normal_matrix pid "normalMatrix";

                   Mesh3d.draw mesh3d)
           | _ -> ()
         in
         List.iter
           (fun c ->
             List.iter (fun (m, s, t) -> render_entity c m s t) entities)
           cameras))

let cleanup =
  Ecs.System.make
    (fun q ->
      let context = q (Ecs.Query.create [ Ecs.Query.Required Context.C.id ]) in
      let shaders = q (Ecs.Query.create [ Ecs.Query.Required Shader.C.id ]) in
      let meshes3d = q (Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ]) in
      ( context |> Ecs.Query.Result.entity_single,
        shaders
        |> Ecs.Query.Result.map (function
             | [ s ] -> Ecs.Component.unpack (module Shader.C) s
             | _ -> assert false),
        meshes3d
        |> Ecs.Query.Result.map (function
             | [ m ] -> Ecs.Component.unpack (module Mesh3d.C) m
             | _ -> assert false) ))
    (Ecs.System.Immediate
       (fun w -> function
         | Some (context_entity, [ context ]), shaders, meshes3d ->
             (* Cleanup context *)
             let context = context |> Ecs.Component.unpack (module Context.C) in
             Context.destroy context;
             Ecs.World.remove_entity w context_entity;
             (* Destroy shaders *)
             List.iter Shader.destroy shaders;
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
  Ecs.World.add_system w Ecs.Scheduler.Update shade3d;

  Ecs.World.add_system w Ecs.Scheduler.Last cleanup
