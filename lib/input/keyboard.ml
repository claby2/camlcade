module Set = Set.Make (struct
  type t = Key.key

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
  let query q =
    let open Ecs.Query in
    let key = q (create [ Required Key_event.C.id ]) in
    let keyboard = q (create [ Required C.id ]) in
    (Result.single key, Result.single keyboard)
  in
  let update key keyboard =
    let keys = Key_event.read key in
    List.iter
      (function
        | Key.Down key -> press keyboard key
        | Key.Up key -> release keyboard key)
      keys
  in
  Ecs.System.make query
    (Ecs.System.Query
       (function
       | Some [ key ], Some [ keyboard ] ->
           let key = key |> Ecs.Component.unpack (module Key_event.C) in
           let keyboard = keyboard |> Ecs.Component.unpack (module C) in
           clear_just_pressed keyboard;
           update key keyboard
       | _ -> assert false))
