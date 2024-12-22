open Ecs

type t = { world : World.t; plugins : (World.t -> unit) list }

let create () = { world = World.create (); plugins = [] }
let add_plugin p a = { a with plugins = p :: a.plugins }

let run a =
  let w =
    List.fold_right
      (fun p w ->
        p w;
        w)
      a.plugins a.world
  in
  World.run_systems w Scheduler.Startup;
  (* TODO: Run main loop *)
  while not (World.has_quit w) do
    World.run_systems w Scheduler.Update
  done;
  World.run_systems w Scheduler.Last
