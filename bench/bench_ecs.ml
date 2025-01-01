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
  Bench.Test.create_with_initialization
    ~name:"component repeated add and remove" (fun _ ->
      let world = World.create () in
      let entity = World.add_entity world in
      fun () ->
        for _ = 1 to 50 do
          World.with_component world (module Foo.C) () entity |> ignore;
          World.remove_component world Foo.C.id entity
        done)

let t3 =
  Bench.Test.create_with_initialization ~name:"big query" (fun _ ->
      let param = 100_000 in
      let world = World.create () in
      for _ = 1 to param do
        World.add_entity world
        |> World.with_component world (module Foo.C) ()
        |> World.with_component world (module Bar.C) ()
        |> ignore
      done;
      fun () ->
        let results =
          World.evaluate_query world
            (Query.create [ Query.Required Foo.C.id; Query.Required Bar.C.id ])
        in
        assert (List.length results = param))

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
