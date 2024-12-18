open Ecs

let test_hash () =
  assert (Archetype.Hash.hash [] = Archetype.Hash.hash []);
  assert (Archetype.Hash.hash [ 1 ] = Archetype.Hash.hash [ 1 ]);
  assert (Archetype.Hash.hash [ 1; 2 ] = Archetype.Hash.hash [ 2; 1 ]);
  assert (Archetype.Hash.hash [ 1; 2 ] <> Archetype.Hash.hash [ 1 ])

let test_edges () =
  let edges = Archetype.Edges.empty in
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

let () =
  test_hash ();
  test_edges ()
