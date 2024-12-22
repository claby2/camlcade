open Util
open Ecs

let test_simple () =
  let w = World.create () in
  let e =
    World.add_entity w
    |> World.with_component w (Component.pack (module Foo.C) (ref 42))
  in

  let value = ref None in
  let simple = function
    | [| [ (e', [ foo ]) ] |] ->
        assert (e' == e);
        let foo = foo |> Component.unpack |> Foo.C.of_base in
        value := Some !foo
    | _ -> assert false
  in

  World.add_system w Scheduler.Update
    [| Query.create [ Query.Required Foo.C.id ] |]
    (System.Query simple);

  World.run_systems w Scheduler.Update;

  assert (!value = Some 42)

let test_order () =
  let w = World.create () in
  let e =
    World.add_entity w
    |> World.with_component w (Component.pack (module Foo.C) (ref 0))
  in

  let set_system n = function
    | [| [ (_, [ foo ]) ] |] ->
        let foo = foo |> Component.unpack |> Foo.C.of_base in
        foo := n
    | _ -> assert false
  in

  let set_one = set_system 1 in
  let set_two = set_system 2 in

  World.add_system w Scheduler.Update
    [| Query.create [ Query.Required Foo.C.id ] |]
    (System.Query set_two);

  World.add_system w Scheduler.Update
    [| Query.create [ Query.Required Foo.C.id ] |]
    (System.Query set_one);

  World.run_systems w Scheduler.Update;

  let foo =
    World.get_component w e Foo.C.id
    |> Option.get |> Component.unpack |> Foo.C.of_base
  in
  assert (!foo = 1)

let test_complex () =
  let w = World.create () in
  let entities =
    List.init 10 (fun v ->
        World.add_entity w
        |> World.with_component w (Component.pack (module Foo.C) (ref v))
        |> World.with_component w
             (Component.pack (module Name.C) (ref "placeholder")))
  in

  let update_entities : Query.Result.t array -> unit = function
    | [| r1; r2 |] ->
        r1
        |> List.iter (function
             | e, [ foo; name; baz ] ->
                 let foo = foo |> Component.unpack |> Foo.C.of_base in
                 let name = name |> Component.unpack |> Name.C.of_base in
                 foo := Id.Entity.to_int e;
                 name := string_of_int (Id.Entity.to_int e);
                 assert (
                   baz |> Component.unpack |> Baz.C.of_base_opt
                   |> Option.is_none)
             | _ -> assert false);
        let baz_is_none = function
          | _, [ baz ] ->
              assert (
                baz |> Component.unpack |> Baz.C.of_base_opt |> Option.is_none)
          | _ -> assert false
        in
        r2 |> List.iter baz_is_none
    | _ -> assert false
  in

  World.add_system w Scheduler.Update
    [|
      Query.create
        [
          Query.Required Foo.C.id;
          Query.Required Name.C.id;
          Query.Optional Baz.C.id;
        ];
      Query.create [ Query.Optional Baz.C.id ];
    |]
    (System.Query update_entities);

  World.run_systems w Scheduler.Update;

  entities
  |> List.iter (fun e ->
         let foo =
           World.get_component w e Foo.C.id
           |> Option.get |> Component.unpack |> Foo.C.of_base
         in
         assert (!foo = Id.Entity.to_int e);
         let name =
           World.get_component w e Name.C.id
           |> Option.get |> Component.unpack |> Name.C.of_base
         in
         assert (int_of_string !name = Id.Entity.to_int e))

let () =
  test_simple ();
  test_order ();
  test_complex ()
