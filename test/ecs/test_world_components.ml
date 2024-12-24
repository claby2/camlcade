open Util
open Ecs

let test_with_component () =
  let w = World.create () in
  let e =
    World.add_entity w
    |> World.with_component w (Component.pack (module Foo.C) (ref 2))
    |> World.with_component w (Component.pack (module Bar.C) (ref 1))
    |> World.with_component w (Component.pack (module Foo.C) (ref 0))
  in
  let e_components = World.get_component w e in
  (match (e_components Foo.C.id, e_components Bar.C.id) with
  | Some foo, Some bar ->
      let foo = foo |> Component.unpack (module Foo.C) in
      assert (!foo = 0);
      let bar = bar |> Component.unpack (module Bar.C) in
      assert (!bar = 1)
  | _ -> assert false);

  assert (e_components Baz.C.id |> Option.is_none)

let test_diabolical_graph () =
  let w = World.create () in
  (* e1 creates archetype graph [] -> [Foo] -> [Foo, Bar] -> [Foo, Bar, Baz] *)
  let e1 =
    World.add_entity w
    |> World.with_component w (Component.pack (module Foo.C) (ref 0))
    |> World.with_component w (Component.pack (module Bar.C) (ref 0))
    |> World.with_component w (Component.pack (module Baz.C) (ref 0))
  in
  (* e2 creates archetype graph [] -> [Bar] -> [Bar, Baz] *)
  let e2 =
    World.add_entity w
    |> World.with_component w (Component.pack (module Bar.C) (ref 0))
    |> World.with_component w (Component.pack (module Baz.C) (ref 0))
  in

  (* When Foo is removed, the world will attempt to traverse the archetype graph
     from [Foo, Bar, Baz] to [Bar, Baz], but this remove edge does not exist yet.
     However, the [Bar, Baz] archetype already exists after e2 was added.
     So, this test ensures that the same archetype is reused. *)
  World.remove_component w Foo.C.id e1;

  let e1_get = World.get_component w e1 in
  assert (e1_get Foo.C.id = None);
  assert (e1_get Bar.C.id |> Option.is_some);
  assert (e1_get Baz.C.id |> Option.is_some);

  (* e2 should still have components [Bar, Baz], if not, it probably means that
     the previous remove_component call inadvertently replaced the [Bar, Baz] archetype *)
  let e2_get = World.get_component w e2 in
  assert (e2_get Bar.C.id |> Option.is_some);
  assert (e2_get Baz.C.id |> Option.is_some);

  (* Now, add back the Foo component to e1 *)
  World.add_component w (Component.pack (module Foo.C) (ref 1)) e1;
  assert (e1_get Foo.C.id |> Option.is_some)

let () =
  test_with_component ();
  test_diabolical_graph ()
