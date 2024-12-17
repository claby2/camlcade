open Ecs

module CustomComponent = Component.Make (struct
  type t = bool
  type Component.component += CustomComponent of t

  let of_component = function
    | CustomComponent t -> t
    | _ -> failwith "Invalid component"

  let to_component t = CustomComponent t
end)

let () =
  let world = World.create in
  let player = World.add_entity world in
  World.add_component world player
    (Component.make (module Component.Transform) (Math.Vec3.make 0. 0. 0.));
  let enemy = World.add_entity world in
  World.add_component world enemy
    (Component.make (module Component.Transform) (Math.Vec3.make 0. 0. 0.));
  World.add_component world enemy (Component.make (module CustomComponent) true);
  World.add_component world player (Component.make (module Component.Health) 0);
  World.print world;
  ()
