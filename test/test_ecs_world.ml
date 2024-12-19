open Ecs

module Foo = struct
  module T = struct
    type t = int
  end

  module C = Component.Make (T)
end

module Switch = struct
  module T = struct
    type t = bool
  end

  module C = Component.Make (T)
end

let test_evaluate_query () =
  let world = World.create () in
  let player = World.add_entity world in
  World.add_component world
    (Component.make (module Component.Transform.C) (Math.Vec3.make 0. 0. 0.))
    player;
  World.add_component world (Component.make (module Foo.C) 0) player;

  let result =
    World.evaluate_query world (Query.create [ Query.Required Switch.C.id ])
  in
  assert (result = []);

  let result =
    World.evaluate_query world (Query.create [ Query.Required Foo.C.id ])
  in
  assert (List.length result = 1);

  let result =
    World.evaluate_query world
      (Query.create [ Query.Required Component.Transform.C.id ])
  in
  assert (List.length result = 1);

  let result =
    World.evaluate_query world
      (Query.create
         [ Query.Required Component.Transform.C.id ]
         ~filter:(Query.Filter.Without Foo.C.id))
  in
  assert (result = [])

let test_systems () =
  let update_transforms (result : Query.Result.t) =
    let rec aux = function
      | [] -> ()
      | (_, [ value ]) :: rest ->
          let transform =
            value |> Component.extract |> Component.Transform.C.of_component
          in
          Math.Vec3.set_x transform (Math.Vec3.x transform +. 1.);
          aux rest
      | _ -> failwith "died"
    in
    aux result
  in
  let world = World.create () in
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
  World.run_systems world System.Update;
  assert (
    World.get_component world Component.Transform.C.id player
    |> Option.get |> Component.extract |> Component.Transform.C.of_component
    = Math.Vec3.make 2. 0. 0.)

let () =
  test_systems ();
  test_evaluate_query ()
