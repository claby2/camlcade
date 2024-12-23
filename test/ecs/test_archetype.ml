open Util
open Ecs

let test_hash () =
  let open Archetype.Hash in
  let ids = List.map Id.Component.of_int in
  assert (hash [] = hash []);
  assert (hash (ids [ 1 ]) = hash (ids [ 1 ]));
  assert (hash (ids [ 1; 2 ]) = hash (ids [ 2; 1 ]));
  assert (hash (ids [ 1; 2 ]) <> hash (ids [ 1 ]));
  assert (
    hash (ids (List.init 20 (fun i -> i)))
    <> hash (ids (List.init 21 (fun i -> i))))

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

let test_hash_component () =
  let a = Archetype.create (Id.ComponentSet.of_list [ Foo.C.id; Baz.C.id ]) in
  assert (Archetype.hash_with_component a Foo.C.id = Archetype.hash a);
  assert (Archetype.hash_with_component a Baz.C.id = Archetype.hash a);
  assert (Archetype.hash_with_component a Bar.C.id <> Archetype.hash a);

  assert (Archetype.hash_without_component a Foo.C.id <> Archetype.hash a);
  assert (Archetype.hash_without_component a Baz.C.id <> Archetype.hash a);
  assert (Archetype.hash_without_component a Bar.C.id = Archetype.hash a)

let test_components () =
  let components = Id.ComponentSet.of_list [ Foo.C.id; Bar.C.id; Baz.C.id ] in
  let a = Archetype.create components in
  assert (Id.ComponentSet.equal (Archetype.components a) components);

  let a = Archetype.empty () in
  assert (Id.ComponentSet.is_empty (Archetype.components a))

let test_add_entity () =
  let a = Archetype.create (Id.ComponentSet.of_list [ Foo.C.id; Bar.C.id ]) in
  let e = Id.Entity.of_int 1 in

  let foo_value e =
    !(Archetype.get_component a e Foo.C.id
     |> Option.get |> Component.unpack |> Foo.C.of_base)
  in
  let bar_value e =
    !(Archetype.get_component a e Bar.C.id
     |> Option.get |> Component.unpack |> Bar.C.of_base)
  in

  Archetype.add_entity a e
    [
      Component.pack (module Foo.C) (ref 0);
      Component.pack (module Bar.C) (ref 1);
    ];
  assert (foo_value e = 0);
  assert (bar_value e = 1);

  Archetype.add_entity a e
    [
      Component.pack (module Bar.C) (ref 24);
      Component.pack (module Foo.C) (ref 42);
    ];
  assert (foo_value e = 42);
  assert (bar_value e = 24);

  assert (Id.EntitySet.cardinal (Archetype.entities a) = 1)

let test_add_entity_exception () =
  let a = Archetype.create (Id.ComponentSet.of_list [ Foo.C.id; Bar.C.id ]) in
  let e = Id.Entity.of_int 1 in

  let is_invalid_arg f =
    try
      f ();
      false
    with Invalid_argument _ -> true
  in

  assert (is_invalid_arg (fun _ -> Archetype.add_entity a e []));
  assert (
    is_invalid_arg (fun _ ->
        Archetype.add_entity a e [ Component.pack (module Baz.C) (ref 0) ]));
  assert (
    is_invalid_arg (fun _ ->
        Archetype.add_entity a e
          [
            Component.pack (module Foo.C) (ref 0);
            Component.pack (module Baz.C) (ref 0);
          ]));
  assert (
    is_invalid_arg (fun _ ->
        Archetype.add_entity a e
          [
            Component.pack (module Foo.C) (ref 0);
            Component.pack (module Bar.C) (ref 0);
            Component.pack (module Baz.C) (ref 0);
          ]))

let test_extract_entity () =
  let a = Archetype.create (Id.ComponentSet.of_list [ Foo.C.id; Bar.C.id ]) in
  let e = Id.Entity.of_int 1 in
  Archetype.add_entity a e
    [
      Component.pack (module Foo.C) (ref 0);
      Component.pack (module Bar.C) (ref 1);
    ];
  let extracted_ids = Archetype.extract_entity a e |> List.map Component.id in
  assert (extracted_ids = [ Foo.C.id; Bar.C.id ]);

  let not_found =
    try
      Archetype.extract_entity a e |> ignore;
      false
    with Not_found -> true
  in
  assert not_found;

  assert (Id.EntitySet.is_empty (Archetype.entities a))

let () =
  test_hash ();
  test_edges ();
  test_hash_component ();
  test_components ();
  test_add_entity ();
  test_add_entity_exception ();
  test_extract_entity ()
