open Core_bench
open Graphics

let t1 =
  Bench.Test.create ~name:"primitive sphere creation" (fun () ->
      Primitive.Sphere.create ~param1:50 ~param2:50 ())

let tests = [ t1 ]
let command = Bench.make_command tests
