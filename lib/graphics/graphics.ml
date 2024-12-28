open Util
open Tgl4
open Tsdl
module Camera = Camera
module Mesh3d = Mesh3d
module Shader = Shader
module Vertex_mesh = Vertex_mesh
module Primitive = Primitive

let initialize ~gl =
  Ecs.System.Query
    (function
    | [| context; shaders; meshes3d |] ->
        (* Initialize context *)
        (match Ecs.Query.Result.single context with
        | Some [ context ] ->
            let context = context |> Ecs.Component.unpack (module Context.C) in
            Context.initialize ~gl context
        | _ -> assert false);
        (* Initialize shaders *)
        Ecs.Query.Result.iter shaders (function
          | [ shader ] ->
              let shader = shader |> Ecs.Component.unpack (module Shader.C) in
              Shader.initialize shader
          | _ -> assert false);
        (* Initialize and install 3d meshes *)
        Ecs.Query.Result.iter meshes3d (function
          | [ mesh3d ] ->
              let mesh3d = mesh3d |> Ecs.Component.unpack (module Mesh3d.C) in
              Mesh3d.initialize mesh3d;
              (* TODO: Is this the right place to install VBOs? *)
              Mesh3d.install_vbo mesh3d
          | _ -> assert false)
    | _ -> assert false)

let render =
  Ecs.System.Query
    (function
    | [| context |] -> (
        match Ecs.Query.Result.single context with
        | Some [ context ] ->
            let context = context |> Ecs.Component.unpack (module Context.C) in
            Context.render context
        | _ -> assert false)
    | _ -> assert false)

let shade3d = function
  | [| cameras; entities |] ->
      check_gl_error ();
      Gl.clear_color 0. 0. 0. 1.;
      Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
      let render_to_camera c =
        let render_entity mesh3d shader t_opt =
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

        (* TODO: Collect all entities that use the phong shader *)
        Ecs.Query.Result.iter entities (function
          | [ mesh3d; shader; transform_opt ] ->
              let mesh3d = mesh3d |> Ecs.Component.unpack (module Mesh3d.C) in
              let shader = shader |> Ecs.Component.unpack (module Shader.C) in
              let transform_opt =
                transform_opt |> Ecs.Component.unpack_opt (module Transform.C)
              in
              render_entity mesh3d shader transform_opt
          | _ -> assert false)
      in
      Ecs.Query.Result.iter cameras (function
        | [ camera ] ->
            let camera =
              camera |> Ecs.Component.unpack (module Camera.Dim3.C)
            in
            render_to_camera camera
        | _ -> assert false)
  | _ -> assert false

let cleanup =
  Ecs.System.Immediate
    (fun w -> function
      | [| context; shaders; meshes3d |] ->
          (* Cleanup context *)
          (match Ecs.Query.Result.entity_single context with
          | Some (context_entity, [ context ]) ->
              let context =
                context |> Ecs.Component.unpack (module Context.C)
              in
              Context.destroy context;
              Ecs.World.remove_entity w context_entity
          | _ -> assert false);
          (* Destroy shaders *)
          Ecs.Query.Result.iter shaders (function
            | [ shader ] ->
                let shader = shader |> Ecs.Component.unpack (module Shader.C) in
                Shader.destroy shader
            | _ -> assert false);
          (* Destroy meshes *)
          Ecs.Query.Result.iter meshes3d (function
            | [ mesh3d ] ->
                let mesh3d = mesh3d |> Ecs.Component.unpack (module Mesh3d.C) in
                Mesh3d.destroy mesh3d
            | _ -> assert false)
      | _ -> assert false)

let plugin w =
  let add_context w =
    Ecs.World.add_entity w
    |> Ecs.World.with_component w (module Context.C) (Context.empty ())
    |> ignore
  in
  add_context w;

  Ecs.World.add_system w Ecs.Scheduler.Startup
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Shader.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    (initialize ~gl:(4, 0));

  Ecs.World.add_system w Ecs.Scheduler.Update
    [| Ecs.Query.create [ Ecs.Query.Required Context.C.id ] |]
    render;

  Ecs.World.add_system w Ecs.Scheduler.Update
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Input.State.Window_events.C.id ];
    |]
    (Ecs.System.Query
       (function
       | [| [ (_, [ context ]) ]; [ (_, [ window_events ]) ] |] ->
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
           done));
  Ecs.World.add_system w Ecs.Scheduler.Update
    [|
      Ecs.Query.create [ Ecs.Query.Required Camera.Dim3.C.id ];
      Ecs.Query.create
        [
          Ecs.Query.Required Mesh3d.C.id;
          Ecs.Query.Required Shader.C.id;
          Ecs.Query.Optional Transform.C.id;
        ];
    |]
    (Ecs.System.Query shade3d);

  Ecs.World.add_system w Ecs.Scheduler.Last
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Shader.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    cleanup
