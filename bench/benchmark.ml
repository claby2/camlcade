open Core

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks" [ ("ecs", Ecs_bench.command) ])

let () = main ()
