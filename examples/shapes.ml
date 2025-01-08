(** Demonstrate the various primitive shapes. *)

open Camlcade
open Ecs

(** Marker component for shapes. *)
module Shape = struct
  type t = unit

  module C = Component.Make (struct
    type inner = t
  end)
end

let rotate =
  let last_timestamp = ref None in
  let query w =
    let transforms =
      World.query ~filter:(Query.Filter.With Shape.C.id) w
        Query.(Req (module Transform.C) @ Nil)
      |> List.map (fun (_, (t, ())) -> t)
    in
    transforms
  in
  let rotate transforms =
    List.iter
      (fun (t : Transform.t) ->
        let last_timestamp =
          match !last_timestamp with
          | None ->
              last_timestamp := Some (Unix.gettimeofday ());
              0.
          | Some t -> t
        in
        Transform.set_rotation t
          (Math.Quat.rot3_zyx
             (Math.Vec3.v 0.0 (Unix.gettimeofday () -. last_timestamp) 0.0)))
      transforms
  in
  System.make query (System.Query rotate)

let add_shape w primitive x y z =
  World.add_entity w
  |> World.with_component w
       (module Graphics.Mesh3d.C)
       (Graphics.Mesh3d.of_primitive primitive)
  |> World.with_component w
       (module Transform.C)
       Transform.(of_xyz x y z |> with_scale (Math.Vec3.v 1.3 1.3 1.3))
  |> World.with_component w (module Graphics.Shader.Normal.C) ()
  |> World.with_component w (module Shape.C) ()

let add_camera w =
  World.add_entity w
  |> World.with_component w (module Graphics.Camera3d.C) ()
  |> World.with_component w
       (module Graphics.Camera.Projection.C)
       (Graphics.Camera.Projection.perspective () ~height_angle:(Float.pi /. 4.))
  |> World.with_component w
       (module Transform.C)
       Transform.(
         identity ()
         |> with_translation (Math.Vec3.v 0. 7. 14.)
         |> with_look_at (Math.Vec3.v 0. 0. 0.))

let plugin w =
  add_camera w |> ignore;
  (* TODO: Use more interesting primitives *)
  let primitives =
    [
      Graphics.Primitive.Sphere.create ~param1:2 ~param2:2 ();
      Graphics.Primitive.Sphere.create ~param1:4 ~param2:4 ();
      Graphics.Primitive.Sphere.create ~param1:8 ~param2:8 ();
      Graphics.Primitive.Sphere.create ~param1:16 ~param2:16 ();
      Graphics.Primitive.Cuboid.create ~param1:16 ();
    ]
  in
  let x_extent = 14.0 in
  let step = x_extent /. Float.of_int (List.length primitives) in
  List.iteri
    (fun i primitive ->
      add_shape w primitive ((Float.of_int i *. step) -. (x_extent /. 2.)) 0. 0.
      |> ignore)
    primitives;

  World.add_system w Scheduler.Update rotate

let () =
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
