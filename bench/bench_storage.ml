(** Compare the performance of [Hashtbl] and [Sparse_set]. *)

open Core_bench
open Storage

let param = 100_000

let b1 =
  Bench.Test.create ~name:"baseline add" (fun () ->
      let table = Hashtbl.create 0 in
      for i = 0 to param do
        Hashtbl.add table i i
      done)

let b2 =
  Bench.Test.create ~name:"baseline remove" (fun () ->
      let table = Hashtbl.create 0 in
      for i = 0 to param do
        Hashtbl.add table i i
      done;
      for i = 0 to param do
        Hashtbl.remove table i
      done)

let b3 =
  Bench.Test.create_with_initialization ~name:"baseline find" (fun _ ->
      let table = Hashtbl.create 0 in
      for i = 0 to param do
        Hashtbl.add table i i
      done;
      fun () ->
        for i = 0 to param do
          Hashtbl.find_opt table i |> ignore
        done)

let t1 =
  Bench.Test.create ~name:"sparse set add" (fun () ->
      let set = Sparse_set.create () in
      for i = 0 to param do
        Sparse_set.set set i i
      done)

let t2 =
  Bench.Test.create ~name:"sparse set remove" (fun () ->
      let set = Sparse_set.create () in
      for i = 0 to param do
        Sparse_set.set set i i
      done;
      for i = 0 to param do
        Sparse_set.remove set i |> ignore
      done)

let t3 =
  Bench.Test.create_with_initialization ~name:"sparse set find" (fun _ ->
      let set = Sparse_set.create () in
      for i = 0 to param do
        Sparse_set.set set i i
      done;
      fun () ->
        for i = 0 to param do
          Sparse_set.get set i |> ignore
        done)

let tests = [ b1; b2; b3; t1; t2; t3 ]
let command = Bench.make_command tests
