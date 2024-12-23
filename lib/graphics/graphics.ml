open Util
open Tgl4
module Camera = Camera
module Mesh = Mesh
module Mesh3d = Mesh3d
module Shader = Shader

let initialize ~gl = function
  | [| [ (_, [ context ]) ]; shaders; meshes3d |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.initialize ~gl context;

      let initialize_shader shader =
        let shader = shader |> Ecs.Component.unpack |> Shader.C.of_base in
        Shader.T.initialize shader
      in
      shaders
      |> List.iter (fun (_, s) ->
             match s with [ s ] -> initialize_shader s | _ -> assert false);

      let initialize_mesh3d mesh3d =
        let mesh3d = mesh3d |> Ecs.Component.unpack |> Mesh3d.C.of_base in
        Mesh3d.T.initialize mesh3d;
        (* TODO: Is this the right place to install VBOs? *)
        Mesh3d.T.install_vbo mesh3d
      in
      meshes3d
      |> List.iter (fun (_, m) ->
             match m with [ m ] -> initialize_mesh3d m | _ -> assert false)
  | _ -> assert false

let render = function
  | [| [ (_, [ context ]) ] |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.render context
  | _ -> assert false

let shade3d = function
  | [| cameras; entities |] ->
      check_gl_error ();
      Gl.clear_color 0. 0. 0. 1.;
      Gl.clear Gl.color_buffer_bit;
      let render_to_camera c =
        let render_entity mesh3d shader =
          match Shader.T.tag_opt shader with
          | Some Shader.Phong ->
              Shader.T.with_shader shader (fun pid ->
                  load_matrix4fv (Camera.Dim3.T.view c) pid "viewMatrix";
                  load_matrix4fv
                    (Camera.Dim3.T.projection c)
                    pid "projectionMatrix";

                  (* TODO: This ctm is currently hardcoded, we should fetch this from the transform *)
                  let ctm = Math.Mat4.identity in
                  let normal_matrix =
                    Math.Mat3.inverse (Math.Mat3.transpose Math.Mat3.identity)
                  in
                  load_matrix4fv ctm pid "modelMatrix";
                  load_matrix3fv normal_matrix pid "normalMatrix";

                  Mesh3d.T.draw mesh3d)
          | _ -> ()
        in

        (* TODO: Collect all entities that use the phong shader *)
        entities
        |> List.iter (fun (_, components) ->
               match components with
               | [ m; s ] ->
                   let m = m |> Ecs.Component.unpack |> Mesh3d.C.of_base in
                   let s = s |> Ecs.Component.unpack |> Shader.C.of_base in
                   render_entity m s
               | _ -> assert false)
      in
      cameras
      |> List.iter (fun (_, c) ->
             match c with
             | [ c ] ->
                 let c = c |> Ecs.Component.unpack |> Camera.Dim3.C.of_base in
                 render_to_camera c
             | _ -> assert false)
  | _ -> assert false

let cleanup w = function
  | [| [ (context_entity, [ context ]) ]; shaders; meshes3d |] ->
      let context = context |> Ecs.Component.unpack |> Context.C.of_base in
      Context.T.destroy context;
      Ecs.World.remove_entity w context_entity;

      let destroy_shader entity shader =
        let shader = shader |> Ecs.Component.unpack |> Shader.C.of_base in
        Shader.T.destroy shader;
        Ecs.World.remove_entity w entity
      in
      shaders
      |> List.iter (fun (e, s) ->
             match s with [ s ] -> destroy_shader e s | _ -> assert false);

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
  add_context w;

  Ecs.World.add_system w Ecs.Scheduler.Startup
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Shader.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    (Ecs.System.Query (initialize ~gl:(4, 0)));

  Ecs.World.add_system w Ecs.Scheduler.Update
    [| Ecs.Query.create [ Ecs.Query.Required Context.C.id ] |]
    (Ecs.System.Query render);

  Ecs.World.add_system w Ecs.Scheduler.Update
    [|
      Ecs.Query.create [ Ecs.Query.Required Camera.Dim3.C.id ];
      Ecs.Query.create
        [ Ecs.Query.Required Mesh3d.C.id; Ecs.Query.Required Shader.C.id ];
    |]
    (Ecs.System.Query shade3d);

  Ecs.World.add_system w Ecs.Scheduler.Last
    [|
      Ecs.Query.create [ Ecs.Query.Required Context.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Shader.C.id ];
      Ecs.Query.create [ Ecs.Query.Required Mesh3d.C.id ];
    |]
    (Ecs.System.Immediate cleanup)
