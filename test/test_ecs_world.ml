open Ecs

module Switch = struct
  module T = struct
    type t = bool
  end

  module C = Component.Make (T)
end

let () =
  let world = World.empty in
  let player = World.add_entity world in
  let enemy = World.add_entity world in
  Printf.printf "Player: %d\n" player;
  Printf.printf "Enemy: %d\n" enemy;
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
  assert (component = None)
