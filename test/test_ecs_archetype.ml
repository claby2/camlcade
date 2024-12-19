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
  let archetype =
    Archetype.create (Id.ComponentSet.of_list [ Bar.C.id; Foo.C.id ])
  in
  Archetype.add_entity archetype 0
    [ Component.pack (module Foo.C) 1; Component.pack (module Bar.C) 2 ];
  Archetype.add_entity archetype 1
    [ Component.pack (module Foo.C) 3; Component.pack (module Bar.C) 4 ];
  assert (
    try
      Archetype.add_entity archetype 3 [];
      false
    with Archetype.Invalid_components -> true);
  assert (
    try
      Archetype.add_entity archetype 3 [ Component.pack (module Foo.C) 5 ];
      false
    with Archetype.Invalid_components -> true);
  assert (
    try
      Archetype.add_entity archetype 3
        [ Component.pack (module Bar.C) 6; Component.pack (module Bar.C) 7 ];
      false
    with Archetype.Invalid_components -> true)

let test_extract_entity () =
  let archetype =
    Archetype.create (Id.ComponentSet.of_list [ Bar.C.id; Foo.C.id ])
  in
  assert (
    try
      Archetype.extract_entity archetype 0 |> ignore;
      false
    with Archetype.Entity_not_found -> true);

  (* Add entity without calling add_entity *)
  Archetype.add_entity archetype 0
    [ Component.pack (module Foo.C) 1; Component.pack (module Bar.C) 2 ];

  assert (
    Archetype.extract_entity archetype 0
    |> List.map Component.id = [ Foo.C.id; Bar.C.id ])

let () =
  test_hash ();
  test_edges ();
  test_add_entity ();
  test_extract_entity ()
