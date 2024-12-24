open Core_bench
open Ecs

module Foo = struct
  type t = unit

  module C = Component.Make (struct
    type inner = t
  end)
end

module Bar = struct
  type t = unit

  module C = Component.Make (struct
    type inner = t
  end)
end

let t1 =
  Bench.Test.create ~name:"entity creation and removal" (fun () ->
      let world = World.create () in
      World.add_entity world |> World.remove_entity world)

let t2 =
  Bench.Test.create ~name:"component repeated add and remove" (fun () ->
      let world = World.create () in
      let entity =
        World.add_entity world
        |> World.with_component world (module Foo.C) ()
        |> World.with_component world (module Bar.C) ()
      in
      for _ = 1 to 50 do
        World.remove_component world Foo.C.id entity;
        World.with_component world (module Foo.C) () entity |> ignore
      done)

let big_world =
  let world = World.create () in
  for _ = 1 to 500 do
    World.add_entity world
    |> World.with_component world (module Foo.C) ()
    |> World.with_component world (module Bar.C) ()
    |> ignore
  done;
  world

let t3 =
  Bench.Test.create ~name:"big query" (fun () ->
      let results =
        World.evaluate_query big_world
          (Query.create [ Query.Required Foo.C.id; Query.Required Bar.C.id ])
      in
      assert (List.length results = 500))

let t4 =
  Bench.Test.create ~name:"register and fetch systems" (fun () ->
      let registry = Scheduler.create () in
      for _ = 1 to 100 do
        let system results = ignore results in
        Scheduler.register registry Scheduler.Update
          ( [|
              Query.create [ Query.Required Foo.C.id; Query.Required Bar.C.id ];
              Query.create [ Query.Required Foo.C.id ];
            |],
            system )
      done;
      Scheduler.fetch registry Scheduler.Update |> ignore)

let tests = [ t1; t2; t3; t4 ]
let command = Bench.make_command tests
