open Util
open Tgl4
module Camera = Camera
module Mesh3d = Mesh3d
module Shader = Shader
module Vertex_mesh = Vertex_mesh
module Primitive = Primitive

let initialize ~gl = function
  | [| [ (_, [ context ]) ]; shaders; meshes3d |] ->
      let context = context |> Ecs.Component.unpack (module Context.C) in
      Context.initialize ~gl context;

      let initialize_shader shader =
        let shader = shader |> Ecs.Component.unpack (module Shader.C) in
        Shader.initialize shader
      in
      shaders
      |> List.iter (fun (_, s) ->
             match s with [ s ] -> initialize_shader s | _ -> assert false);

      let initialize_mesh3d mesh3d =
        let mesh3d = mesh3d |> Ecs.Component.unpack (module Mesh3d.C) in
        Mesh3d.initialize mesh3d;
        (* TODO: Is this the right place to install VBOs? *)
        Mesh3d.install_vbo mesh3d
      in
      meshes3d
      |> List.iter (fun (_, m) ->
             match m with [ m ] -> initialize_mesh3d m | _ -> assert false)
  | _ -> assert false

let render = function
  | [| [ (_, [ context ]) ] |] ->
      let context = context |> Ecs.Component.unpack (module Context.C) in
      Context.render context
  | _ -> assert false

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
        entities
        |> List.iter (fun (_, components) ->
               match components with
               | [ m; s; t_opt ] ->
                   let m = m |> Ecs.Component.unpack (module Mesh3d.C) in
                   let s = s |> Ecs.Component.unpack (module Shader.C) in
                   let t_opt =
                     t_opt |> Ecs.Component.unpack_opt (module Transform.C)
                   in
                   render_entity m s t_opt
               | _ -> assert false)
      in
      cameras
      |> List.iter (fun (_, c) ->
             match c with
             | [ c ] ->
                 let c = c |> Ecs.Component.unpack (module Camera.Dim3.C) in
                 render_to_camera c
             | _ -> assert false)
  | _ -> assert false

let cleanup w = function
  | [| [ (context_entity, [ context ]) ]; shaders; meshes3d |] ->
      let context = context |> Ecs.Component.unpack (module Context.C) in
      Context.destroy context;
      Ecs.World.remove_entity w context_entity;

      let destroy_shader entity shader =
        let shader = shader |> Ecs.Component.unpack (module Shader.C) in
        Shader.destroy shader;
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
                   mesh3d |> Ecs.Component.unpack (module Mesh3d.C)
                 in
                 Mesh3d.destroy mesh3d
             | _ -> assert false)
  | _ -> assert false

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
    (Ecs.System.Query (initialize ~gl:(4, 0)));

  Ecs.World.add_system w Ecs.Scheduler.Update
    [| Ecs.Query.create [ Ecs.Query.Required Context.C.id ] |]
    (Ecs.System.Query render);

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
    (Ecs.System.Immediate cleanup)
