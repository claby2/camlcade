open Ecs

module Foo = struct
  module T = struct
    type t = int
  end

  module C = Component.Make (T)
end

module Bar = struct
  module T = struct
    type t = int
  end

  module C = Component.Make (T)
end

module Switch = struct
  module T = struct
    type t = bool ref

    let toggle t = t := not !t
  end

  module C = Component.Make (T)
end

let test_entity_lifecycle () =
  let world = World.create () in
  (* Create an entity with Foo and Switch components *)
  let e1 =
    World.add_entity world
    |> World.with_component world (Component.pack (module Foo.C) 0)
    |> World.with_component world (Component.pack (module Switch.C) (ref false))
  in
  assert (
    World.get_component world Foo.C.id e1
    |> Option.get |> Component.unpack |> Foo.C.of_base = 0);
  assert (
    !(World.get_component world Switch.C.id e1
     |> Option.get |> Component.unpack |> Switch.C.of_base)
    = false);

  (* Create an entity with only the Switch component *)
  let e2 =
    World.add_entity world
    |> World.with_component world (Component.pack (module Switch.C) (ref true))
  in
  (* No Foo component *)
  assert (World.get_component world Foo.C.id e2 = None);
  assert (
    !(World.get_component world Switch.C.id e2
     |> Option.get |> Component.unpack |> Switch.C.of_base)
    = true);

  World.remove_component world Switch.C.id e2;
  assert (World.get_component world Switch.C.id e2 = None);

  World.remove_entity world e1;
  assert (World.get_component world Foo.C.id e1 = None);
  assert (World.get_component world Switch.C.id e1 = None)

let test_diabolical_archetype_graph () =
  let world = World.create () in
  (* e1 creates archetype graph [] -> [Foo] -> [Foo, Bar] -> [Foo, Bar, Switch] *)
  let e1 =
    World.add_entity world
    |> World.with_component world (Component.pack (module Foo.C) 0)
    |> World.with_component world (Component.pack (module Bar.C) 1)
    |> World.with_component world (Component.pack (module Switch.C) (ref false))
  in
  (* e2 creates archetype graph [] -> [Bar] -> [Bar, Switch] *)
  let e2 =
    World.add_entity world
    |> World.with_component world (Component.pack (module Bar.C) 2)
    |> World.with_component world (Component.pack (module Switch.C) (ref true))
  in

  (* When Foo is removed, the world will attempt to traverse the archetype graph
     from [Foo, Bar, Switch] to [Bar, Switch], but this remove edge does not exist yet.
     However, the [Bar, Switch] archetype already exists after e2 was added.
     So, this test ensures that the same archetype is reused. *)
  World.remove_component world Foo.C.id e1;
  (* e1 should now have components [Bar, Switch] *)
  assert (World.get_component world Foo.C.id e1 = None);
  assert (World.get_component world Bar.C.id e1 |> Option.is_some);
  assert (World.get_component world Switch.C.id e1 |> Option.is_some);

  (* e2 should still have components [Bar, Switch], if not, it probably means that
     the previous remove_component call inadvertently replaced the [Bar, Switch] archetype *)
  assert (World.get_component world Bar.C.id e2 |> Option.is_some);
  assert (World.get_component world Switch.C.id e2 |> Option.is_some)

let test_evaluate_query () =
  let world = World.create () in

  (* Add an entity *)
  World.add_entity world
  |> World.with_component world
       (Component.pack (module Component.Transform.C) (Math.Vec3.make 0. 0. 0.))
  |> World.with_component world (Component.pack (module Foo.C) 0)
  |> ignore;

  (* The entity does not have the Switch component *)
  let result =
    World.evaluate_query world (Query.create [ Query.Required Switch.C.id ])
  in
  assert (result = []);

  (* The entity has the Foo component *)
  let result =
    World.evaluate_query world (Query.create [ Query.Required Foo.C.id ])
  in
  assert (List.length result = 1);

  let result =
    World.evaluate_query world
      (Query.create
         [ Query.Required Component.Transform.C.id; Query.Required Foo.C.id ])
  in
  assert (List.length result = 1);
  assert (List.length (snd (List.hd result)) = 2);

  let result =
    World.evaluate_query world
      (Query.create
         [ Query.Required Component.Transform.C.id ]
         ~filter:(Query.Filter.Without Foo.C.id))
  in
  assert (result = [])

let test_systems () =
  let world = World.create () in
  let spawn_enemy world =
    World.add_entity world
    |> World.with_component world
         (Component.pack
            (module Component.Transform.C)
            (Math.Vec3.make 0. 0. 0.))
    |> World.with_component world (Component.pack (module Switch.C) (ref false))
    |> World.with_component world (Component.pack (module Foo.C) 0)
  in
  let assert_enemies enemies f =
    enemies
    |> List.iter (fun e ->
           let switch =
             World.get_component world Switch.C.id e
             |> Option.get |> Component.unpack |> Switch.C.of_base
           in
           let transform =
             World.get_component world Component.Transform.C.id e
             |> Option.get |> Component.unpack |> Component.Transform.C.of_base
           in
           f e switch transform)
  in

  (* Spawn 10 enemies *)
  let enemies = List.init 10 (fun _ -> spawn_enemy world) in

  (* Define a system and add it to the world *)
  let update_enemies (result : Query.Result.t array) =
    result.(0)
    |> List.iter (function
         | e, [ switch; transform ] ->
             let switch = switch |> Component.unpack |> Switch.C.of_base in
             let transform =
               transform |> Component.unpack |> Component.Transform.C.of_base
             in
             (* Toggle the Switch component *)
             Switch.T.toggle switch;
             (* Set the x-coordinate of the Transform component to the entity id *)
             Math.Vec3.set_x transform (float_of_int e)
         | _ -> assert false)
  in
  World.add_system world System.Update
    [|
      Query.create
        [ Query.Required Switch.C.id; Query.Required Component.Transform.C.id ]
        ~filter:(Query.Filter.With Foo.C.id);
    |]
    update_enemies;

  (* Ensure all enemies Switch and Transform components are unchanged *)
  assert_enemies enemies (fun _ switch transform ->
      assert (!switch = false);
      assert (transform = Math.Vec3.zero));

  World.run_systems world System.Update;

  (* Ensure all enemies Switch component is now true and Transform component is updated *)
  assert_enemies enemies (fun e switch transform ->
      assert (!switch = true);
      assert (Math.Vec3.x transform = float_of_int e));

  (* Add an entity without a transform component *)
  World.add_entity world
  |> World.with_component world (Component.pack (module Switch.C) (ref false))
  |> ignore;

  (* Indicates whether we encountered a None component in place of a Transform component.
     This should be true after running the system since the query should match with the entity
     that was added above, but it does not have a Transform component *)
  let encountered_none = ref false in

  let toggle_optional_switches (result : Query.Result.t array) =
    result.(0)
    |> List.iter (function
         | e, [ _; transform ] -> (
             match
               Component.unpack transform |> Component.Transform.C.of_base_opt
             with
             | Some transform -> Math.Vec3.set_z transform (float_of_int e)
             | _ -> encountered_none := true)
         | _ -> assert false)
  in
  (* Query for entities with an optional Switch and optional Transform component *)
  World.add_system world System.Update
    [|
      Query.create
        [ Query.Optional Switch.C.id; Query.Optional Component.Transform.C.id ];
    |]
    toggle_optional_switches;

  World.run_systems world System.Update;

  assert_enemies enemies (fun e _ transform ->
      assert (Math.Vec3.z transform = float_of_int e));
  (* Ensure that the system encountered a None component *)
  assert !encountered_none

let test_multiple_query_systems () =
  let world = World.create () in
  let foo_bars =
    List.init 11 (fun i ->
        World.add_entity world
        |> World.with_component world (Component.pack (module Foo.C) i)
        |> World.with_component world (Component.pack (module Bar.C) i))
  in
  let switches =
    List.init 6 (fun i ->
        World.add_entity world
        |> World.with_component world (Component.pack (module Foo.C) i)
        |> World.with_component world
             (Component.pack (module Switch.C) (ref false)))
  in
  let foo_bars_count = ref 0 in
  let switches_count = ref 0 in
  let update (result : Query.Result.t array) =
    match result with
    | [| foo_bars; switches |] ->
        (* Update foo_bars *)
        foo_bars
        |> List.iter (function
             | _, [ foo; bar ] ->
                 foo |> Component.unpack |> Foo.C.of_base |> ignore;
                 bar |> Component.unpack |> Bar.C.of_base |> ignore;
                 foo_bars_count := !foo_bars_count + 1
             | _ -> assert false);
        (* Update switches *)
        switches
        |> List.iter (function
             | _, [ switch ] ->
                 switch |> Component.unpack |> Switch.C.of_base |> ignore;
                 switches_count := !switches_count + 1
             | _ -> assert false)
    | _ -> assert false
  in

  World.add_system world System.Update
    [|
      Query.create [ Query.Required Foo.C.id; Query.Required Bar.C.id ];
      Query.create [ Query.Required Switch.C.id ];
    |]
    update;

  World.run_systems world System.Update;

  assert (!foo_bars_count = List.length foo_bars);
  assert (!switches_count = List.length switches)

let test_system_order () =
  let world = World.create () in
  let entity =
    World.add_entity world
    |> World.with_component world
         (Component.pack (module Component.Transform.C) Math.Vec3.zero)
  in
  let create_system x = function
    | [| result |] ->
        result
        |> List.iter (function
             | _, [ transform ] ->
                 let transform =
                   transform |> Component.unpack
                   |> Component.Transform.C.of_base
                 in
                 Math.Vec3.set_x transform x
             | _ -> assert false)
    | _ -> assert false
  in
  let system1 = create_system 1. in
  let system2 = create_system 2. in

  (* Add system2 then system1 after, ideally, system2 should run first *)
  World.add_system world System.Update
    [| Query.create [ Query.Required Component.Transform.C.id ] |]
    system2;
  World.add_system world System.Update
    [| Query.create [ Query.Required Component.Transform.C.id ] |]
    system1;

  World.run_systems world System.Update;

  let transform =
    World.get_component world Component.Transform.C.id entity
    |> Option.get |> Component.unpack |> Component.Transform.C.of_base
  in
  assert (Math.Vec3.x transform = 1.)

let () =
  test_entity_lifecycle ();
  test_diabolical_archetype_graph ();
  test_evaluate_query ();
  test_systems ();
  test_multiple_query_systems ();
  test_system_order ()
