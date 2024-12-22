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

let test_immediate () =
  let w = World.create () in
  let original_entity =
    World.add_entity w
    |> World.with_component w (Component.pack (module Name.C) (ref "whatever"))
  in

  assert (List.length (World.entities w) = 1);

  let spawn_more_entities world : Query.Result.t array -> unit = function
    | _ -> (
        for _ = 1 to 10 do
          World.add_entity world
          |> World.add_component world (Component.pack (module Foo.C) (ref 0))
        done;
        try World.remove_entity world original_entity with _ -> ())
  in

  World.add_system w Scheduler.Update
    [| Query.create [] |]
    (System.Immediate spawn_more_entities);

  World.run_systems w Scheduler.Update;

  assert (List.length (World.entities w) = 10);

  World.run_systems w Scheduler.Update;

  assert (List.length (World.entities w) = 20)

let test_quit () =
  let w = World.create () in

  assert (not (World.has_quit w));

  let quit_system = function _ -> raise World.Quit in
  World.add_system w Scheduler.Update [||] (System.Query quit_system);

  World.run_systems w Scheduler.Update;

  assert (World.has_quit w)

let () =
  test_simple ();
  test_order ();
  test_complex ();
  test_immediate ();
  test_quit ()
