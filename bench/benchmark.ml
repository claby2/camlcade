open Core

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks"
       [
         ("ecs", Bench_ecs.command);
         ("graphics", Bench_graphics.command);
         ("math", Bench_math.command);
         ("storage", Bench_storage.command);
       ])

let () = main ()
