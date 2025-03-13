module type S = sig
  type button
  type t

  val create : unit -> t
  val is_pressed : t -> button -> bool
  val is_just_pressed : t -> button -> bool
  val is_just_released : t -> button -> bool

  module C : Ecs.Component.S with type t = t

  val update_system : Ecs.World.t Ecs.System.t
end

module Make (B : Button.S) (E : Ecs.Event.S with type event = B.t) :
  S with type button = B.button = struct
  type button = B.button

  module Set = Set.Make (struct
    type t = button

    let compare = compare
  end)

  type t = {
    mutable pressed : Set.t;
    mutable just_pressed : Set.t;
    mutable just_released : Set.t;
  }

  let create () =
    { pressed = Set.empty; just_pressed = Set.empty; just_released = Set.empty }

  let press t key =
    let already_pressed = Set.mem key t.pressed in
    t.pressed <- Set.add key t.pressed;
    t.just_pressed <-
      (if already_pressed then t.just_pressed else Set.add key t.just_pressed)

  let clear_just_pressed t = t.just_pressed <- Set.empty

  let release t key =
    let already_pressed = Set.mem key t.pressed in
    t.pressed <- Set.remove key t.pressed;
    t.just_pressed <- Set.remove key t.just_pressed;
    t.just_released <-
      (if already_pressed then Set.add key t.just_released else t.just_released)

  let is_pressed t key = Set.mem key t.pressed
  let is_just_pressed t key = Set.mem key t.just_pressed
  let is_just_released t key = Set.mem key t.just_released

  module C = Ecs.Component.Make (struct
    type inner = t
  end)

  let update_system =
    let query w =
      let open Ecs in
      let _, (event, ()) =
        World.query w Query.[Req (module E.C)] |> List.hd
      in
      let _, (state, ()) =
        World.query w Query.[Req (module C)] |> List.hd
      in
      (event, state)
    in
    let update event state =
      let keys = E.read event in
      List.iter
        (function B.Down e -> press state e | B.Up e -> release state e)
        keys
    in
    Ecs.System.make query
      (Ecs.System.Query
         (fun (event, state) ->
           clear_just_pressed state;
           update event state))
end
