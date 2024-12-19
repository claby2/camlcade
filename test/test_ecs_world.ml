open Ecs

module Switch = struct
  module T = struct
    type t = bool
  end

  module C = Component.Make (T)
end

let update_transforms (result : Query.Result.t) =
  let rec aux = function
    | [] -> ()
    | (entity, [ value ]) :: rest ->
        let transform =
          value |> Component.extract |> Component.Transform.C.of_component
        in
        Printf.printf "Updating entity %d with transform %s\n" entity
          (Math.Vec3.to_string transform);
        (* TODO: Update transform *)
        aux rest
    | _ -> failwith "died"
  in
  aux result

let () =
  let world = World.empty in
  let player = World.add_entity world in
  let enemy = World.add_entity world in
  assert (player <> enemy);
  World.add_component world
    (Component.make (module Component.Transform.C) (Math.Vec3.make 0. 0. 0.))
    player;
  let component = World.get_component world Component.Transform.C.id player in
  assert (
    Option.get component |> Component.extract
    |> Component.Transform.C.of_component = Math.Vec3.zero);
  World.remove_component world Component.Transform.C.id player;
  let component = World.get_component world Component.Transform.C.id player in
  assert (component = None);
  World.add_component world (Component.make (module Switch.C) false) player;
  World.add_component world (Component.make (module Switch.C) true) player;
  let component = World.get_component world Switch.C.id player in
  assert (
    Option.get component |> Component.extract |> Switch.C.of_component = true);
  World.remove_component world Switch.C.id player;
  let component = World.get_component world Switch.C.id player in
  assert (component = None);
  World.add_system world System.Update
    (Query.create [ Query.Required Component.Transform.C.id ])
    update_transforms;
  World.run_systems world System.Update;
  ()
