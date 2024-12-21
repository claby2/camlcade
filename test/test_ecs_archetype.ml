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

let test_hash () =
  assert (Archetype.Hash.hash [] = Archetype.Hash.hash []);
  assert (Archetype.Hash.hash [ 1 ] = Archetype.Hash.hash [ 1 ]);
  assert (Archetype.Hash.hash [ 1; 2 ] = Archetype.Hash.hash [ 2; 1 ]);
  assert (Archetype.Hash.hash [ 1; 2 ] <> Archetype.Hash.hash [ 1 ])

let test_edges () =
  let edges = Archetype.Edges.empty () in
  assert (Archetype.Edges.find_add_opt edges 0 = None);
  assert (Archetype.Edges.find_remove_opt edges 0 = None);

  Archetype.Edges.replace_add edges 0 (Some 3);
  assert (Archetype.Edges.find_add_opt edges 0 = Some 3);
  Archetype.Edges.replace_add edges 0 None;
  assert (Archetype.Edges.find_add_opt edges 0 = None);

  Archetype.Edges.replace_remove edges 0 (Some 2);
  assert (Archetype.Edges.find_remove_opt edges 0 = Some 2);
  Archetype.Edges.replace_remove edges 0 None;
  assert (Archetype.Edges.find_remove_opt edges 0 = None)

let test_add_entity () =
  (* Create an archetype with Foo and Bar components *)
  let archetype =
    Archetype.create (Id.ComponentSet.of_list [ Bar.C.id; Foo.C.id ])
  in

  (* Add two entities to the archetype *)
  Archetype.add_entity archetype 0
    [ Component.pack (module Foo.C) 1; Component.pack (module Bar.C) 2 ];
  Archetype.add_entity archetype 1
    [ Component.pack (module Foo.C) 3; Component.pack (module Bar.C) 4 ];
  assert (Id.EntitySet.cardinal (Archetype.entities archetype) = 2);

  (* Add the same entity again, the number of entities should not change *)
  Archetype.add_entity archetype 1
    [ Component.pack (module Foo.C) 3; Component.pack (module Bar.C) 4 ];
  assert (Id.EntitySet.cardinal (Archetype.entities archetype) = 2);

  (* Adding entities with different sets of components should raise exceptions *)
  assert (
    try
      Archetype.add_entity archetype 3 [];
      false
    with Invalid_argument _ -> true);
  assert (
    try
      Archetype.add_entity archetype 3 [ Component.pack (module Foo.C) 5 ];
      false
    with Invalid_argument _ -> true);
  assert (
    try
      Archetype.add_entity archetype 3
        [ Component.pack (module Bar.C) 6; Component.pack (module Bar.C) 7 ];
      false
    with Invalid_argument _ -> true);

  (* Add an entity with duplicate components, should still work *)
  Archetype.add_entity archetype 3
    [
      Component.pack (module Foo.C) 1;
      Component.pack (module Bar.C) 1;
      (* The next two components should overwrite the previous ones *)
      Component.pack (module Bar.C) 2;
      Component.pack (module Foo.C) 2;
    ];
  assert (Id.EntitySet.cardinal (Archetype.entities archetype) = 3);
  assert (
    Archetype.get_component archetype Foo.C.id 3
    |> Option.get |> Component.unpack |> Foo.C.of_base = 2);
  assert (
    Archetype.get_component archetype Bar.C.id 3
    |> Option.get |> Component.unpack |> Bar.C.of_base = 2)

let test_extract_entity () =
  (* Create an archetype with Foo and Bar components *)
  let archetype =
    Archetype.create (Id.ComponentSet.of_list [ Bar.C.id; Foo.C.id ])
  in
  assert (
    try
      Archetype.extract_entity archetype 0 |> ignore;
      false
    with Not_found -> true);

  (* Add an entity to the archetype *)
  Archetype.add_entity archetype 0
    [ Component.pack (module Foo.C) 1; Component.pack (module Bar.C) 2 ];

  (* Extract the entity and check that it has the correct components *)
  let extracted = Archetype.extract_entity archetype 0 in
  assert (extracted |> List.map Component.id = [ Foo.C.id; Bar.C.id ]);
  assert (Id.EntitySet.cardinal (Archetype.entities archetype) = 0);

  (* Add back the entity and check that the entity set has one entity *)
  Archetype.add_entity archetype 0 extracted;
  assert (Id.EntitySet.cardinal (Archetype.entities archetype) = 1)

let () =
  test_hash ();
  test_edges ();
  test_add_entity ();
  test_extract_entity ()
