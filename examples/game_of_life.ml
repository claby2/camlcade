open Camlcade
open Ecs

module Grid = struct
  type t = { dimension : int; mutable cells : bool array }

  let get t x y z =
    t.cells.((z * t.dimension * t.dimension) + (y * t.dimension) + x)

  let is_alive_at t x y z =
    try
      let v = get t x y z in
      v
    with _ -> false

  let count_neighbors t x y z =
    let count = ref 0 in
    for dx = -1 to 1 do
      for dy = -1 to 1 do
        for dz = -1 to 1 do
          if dx = 0 && dy = 0 && dz = 0 then ()
          else
            let nx = x + dx in
            let ny = y + dy in
            let nz = z + dz in
            if is_alive_at t nx ny nz then count := !count + 1
        done
      done
    done;
    !count

  let init ?(dimension = 16) () =
    {
      dimension;
      cells =
        Array.init (dimension * dimension * dimension) (fun _ -> Random.bool ());
    }

  let randomize t =
    t.cells <- Array.init (Array.length t.cells) (fun _ -> Random.bool ())

  let step t =
    let new_cells = Array.copy t.cells in
    let set_new x y z v =
      Array.set new_cells
        ((z * t.dimension * t.dimension) + (y * t.dimension) + x)
        v
    in
    for x = 0 to t.dimension - 1 do
      for y = 0 to t.dimension - 1 do
        for z = 0 to t.dimension - 1 do
          let neighbors = count_neighbors t x y z in
          let is_alive = is_alive_at t x y z in
          if is_alive then (if neighbors < 13 then set_new x y z false)
          else if neighbors >= 14 && neighbors <= 19 then set_new x y z true
          else set_new x y z is_alive
        done
      done
    done;
    t.cells <- new_cells

  let update_vm t vertex_mesh =
    (* TODO: only add visible faces/vertices. *)
    let open Graphics in
    let data = ref [] in
    for x = 0 to t.dimension - 1 do
      for y = 0 to t.dimension - 1 do
        for z = 0 to t.dimension - 1 do
          let is_alive = is_alive_at t x y z in
          if is_alive then
            let cube = Primitive.Cuboid.create () in
            let rec shift = function
              | [] -> []
              | px :: py :: pz :: nx :: ny :: nz :: rest ->
                  (px +. float_of_int x -. (float_of_int t.dimension /. 2.))
                  :: (py +. float_of_int y -. (float_of_int t.dimension /. 2.))
                  :: (pz +. float_of_int z -. (float_of_int t.dimension /. 2.))
                  :: nx :: ny :: nz :: shift rest
              | _ -> failwith "Invalid list"
            in
            data := shift cube @ !data
        done
      done
    done;
    Vertex_mesh.set_data vertex_mesh (Array.of_list !data)

  module C = Component.Make (struct
    type inner = t
  end)
end

let step =
  let query w =
    let _, (keyboard, ()) =
      World.query w Query.(Req (module Input.Keyboard.C) @ Nil) |> List.hd
    in
    let _, (grid, (mesh, ())) =
      World.query w
        Query.(Req (module Grid.C) @ Req (module Graphics.Mesh3d.C) @ Nil)
      |> List.hd
    in
    (keyboard, grid, mesh)
  in
  let step (keyboard, grid, mesh) =
    let pressed = Input.Keyboard.is_pressed keyboard in
    if pressed `T then (
      Grid.step grid;
      Grid.update_vm grid (Graphics.Mesh3d.vertex_mesh mesh);
      Graphics.Mesh3d.install mesh)
    else if pressed `R then (
      Grid.randomize grid;
      Grid.update_vm grid (Graphics.Mesh3d.vertex_mesh mesh);
      Graphics.Mesh3d.install mesh)
  in
  System.make query (System.Query step)

let spawn_grid w =
  let open Graphics in
  let grid = Grid.init () in
  let vm = Vertex_mesh.create ~topology:TriangleList () in
  Vertex_mesh.set_attribute vm 0 3;
  Vertex_mesh.set_attribute vm 1 3;
  World.add_entity w
  |> World.with_component w (module Grid.C) grid
  |> World.with_component w (module Transform.C) (Transform.identity ())
  |> World.with_component w (module Mesh3d.C) (Mesh3d.of_vertex_mesh vm)
  |> World.with_component w
       (module Material.C)
       (Material.create ~ambient:(Math.Vec3.v 0. 0. 0.) ~shininess:0.1 ())
  |> World.with_component w (module Shader.Phong.C) ()
  |> ignore

let plugin w =
  spawn_grid w;

  let _sun =
    World.add_entity w
    |> World.with_component w
         (module Graphics.Light.Directional.C)
         (Graphics.Light.Directional.create ~color:(Math.Vec3.v 1. 1. 1.) ())
    |> World.with_component w (module Transform.C) (Transform.identity ())
  in

  let _camera =
    World.add_entity w
    |> World.with_component w (module Graphics.Camera3d.C) ()
    |> World.with_component w
         (module Graphics.Camera.Projection.C)
         (Graphics.Camera.Projection.orthographic ~left:(-15.) ~right:15.
            ~bottom:(-15.) ~top:15. ())
    |> World.with_component w
         (module Transform.C)
         Transform.(
           identity ()
           |> with_translation (Math.Vec3.v 8. 20. 25.)
           |> with_look_at Math.Vec3.zero)
  in
  World.add_system w Scheduler.Update step

let () =
  Random.self_init ();
  let app =
    App.create ()
    |> App.add_plugin Input.plugin
    |> App.add_plugin Graphics.plugin
    |> App.add_plugin plugin
  in

  App.run app
