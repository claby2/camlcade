open Util
open Ecs

let test_empty () =
  let w = World.create () in
  (* Making sure that evaluating queries on an empty world creates empty results *)
  assert (World.evaluate_query w (Query.create []) = []);
  assert (
    World.evaluate_query w
      (Query.create [] ~filter:(Query.Filter.Without Foo.C.id))
    = []);
  assert (World.evaluate_query w (Query.create [ Query.Required Foo.C.id ]) = [])

let test_multiple_entities () =
  let w = World.create () in

  (* Add entities *)
  for _ = 1 to 5 do
    World.add_entity w
    |> World.with_component w (module Foo.C) (ref 0)
    |> ignore;
    World.add_entity w
    |> World.with_component w (module Foo.C) (ref 0)
    |> World.with_component w (module Bar.C) (ref 0)
    |> ignore;
    World.add_entity w
    |> World.with_component w (module Foo.C) (ref 0)
    |> World.with_component w (module Bar.C) (ref 0)
    |> World.with_component w (module Baz.C) (ref 0)
    |> ignore
  done;

  let q = World.evaluate_query w in

  assert (q (Query.create []) = []);

  (* All entities have Foo *)
  assert (List.length (q (Query.create [ Query.Required Foo.C.id ])) = 15);

  let res =
    q (Query.create [ Query.Optional Foo.C.id; Query.Required Bar.C.id ])
  in
  assert (List.length res = 10);
  Query.Result.iter
    (function
      | [ f; b ] ->
          (* All entities have Foo, so none should be Component.None *)
          assert (Option.is_some (f |> Component.unpack_opt (module Foo.C)));
          assert (Option.is_some (b |> Component.unpack_opt (module Bar.C)))
      | _ -> assert false)
    res;

  assert (
    List.length
      (q (Query.create [ Query.Required Foo.C.id; Query.Required Baz.C.id ]))
    = 5)

let () =
  test_empty ();
  test_multiple_entities ()
