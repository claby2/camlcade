open Ecs

let test_start_empty () =
  let w = World.create () in
  assert (World.entities w = [])

let test_uniqueness () =
  let w = World.create () in
  assert (World.add_entity w <> World.add_entity w)

let test_add_entity () =
  let w = World.create () in
  let e = World.add_entity w in
  assert (World.entities w = [ e ]);
  for _ = 1 to 99 do
    World.add_entity w |> ignore
  done;
  assert (List.length (World.entities w) = 100)

let test_remove_entity () =
  let w = World.create () in
  let e = World.add_entity w in
  World.remove_entity w e;
  assert (List.is_empty (World.entities w));

  assert (
    try
      World.remove_entity w e;
      false
    with Not_found -> true)

let () =
  test_start_empty ();
  test_uniqueness ();
  test_add_entity ();
  test_remove_entity ()
