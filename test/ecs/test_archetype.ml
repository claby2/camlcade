open Util
open Ecs

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
    !(Archetype.query a e Foo.C.id
     |> Option.get
     |> Component.unpack (module Foo.C))
  in
  let bar_value e =
    !(Archetype.query a e Bar.C.id
     |> Option.get
     |> Component.unpack (module Bar.C))
  in

  Archetype.add a e
    [
      Component.pack (module Foo.C) (ref 0);
      Component.pack (module Bar.C) (ref 1);
    ];
  assert (foo_value e = 0);
  assert (bar_value e = 1);

  Archetype.add a e
    [
      Component.pack (module Bar.C) (ref 24);
      Component.pack (module Foo.C) (ref 42);
    ];
  assert (foo_value e = 42);
  assert (bar_value e = 24)

let test_add_entity_exception () =
  let a = Archetype.create (Id.ComponentSet.of_list [ Foo.C.id; Bar.C.id ]) in
  let e = Id.Entity.of_int 1 in

  let is_invalid_arg f =
    try
      f ();
      false
    with Invalid_argument _ -> true
  in

  assert (is_invalid_arg (fun _ -> Archetype.add a e []));
  assert (
    is_invalid_arg (fun _ ->
        Archetype.add a e [ Component.pack (module Baz.C) (ref 0) ]));
  assert (
    is_invalid_arg (fun _ ->
        Archetype.add a e
          [
            Component.pack (module Foo.C) (ref 0);
            Component.pack (module Baz.C) (ref 0);
          ]));
  assert (
    is_invalid_arg (fun _ ->
        Archetype.add a e
          [
            Component.pack (module Foo.C) (ref 0);
            Component.pack (module Bar.C) (ref 0);
            Component.pack (module Baz.C) (ref 0);
          ]))

let () =
  test_components ();
  test_add_entity ();
  test_add_entity_exception ()
