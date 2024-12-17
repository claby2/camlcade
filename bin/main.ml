open Ecs

module CustomComponent = Component.Make (struct
  type t = bool
  type Component.component += CustomComponent of t

  let of_component = function
    | CustomComponent t -> t
    | _ -> failwith "Invalid component"

  let to_component t = CustomComponent t
end)

let print_transform_system (entities : World.entity_store) =
  Hashtbl.iter
    (fun _ components ->
      components
      |> Hashtbl.iter (fun _ component ->
             let c = Component.extract component in
             match c with
             | Component.Transform.T t ->
                 Printf.printf "- Transform: %s\n" (Math.Vec3.to_string t)
             | _ -> ()))
    entities

let increment_transforms (entities : World.entity_store) =
  Hashtbl.iter
    (fun _ components ->
      components
      |> Hashtbl.iter (fun _ component ->
             let c = Component.extract component in
             match c with
             | Component.Transform.T t -> Math.Vec3.set_x t (Math.Vec3.x t +. 1.)
             | _ -> ()))
    entities

let () =
  let world = World.create in
  let player = World.add_entity world in
  World.add_component world player
    (Component.make (module Component.TransformC) (Math.Vec3.make 0. 0. 0.));
  let enemy = World.add_entity world in
  World.add_component world enemy
    (Component.make (module Component.TransformC) (Math.Vec3.make 0. 0. 0.));
  World.add_component world enemy (Component.make (module CustomComponent) true);
  World.add_component world player (Component.make (module Component.HealthC) 0);
  World.print world;
  World.add_system world print_transform_system;
  World.add_system world increment_transforms;
  World.run_systems world;
  World.run_systems world;
  ()
