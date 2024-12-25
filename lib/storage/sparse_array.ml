open Containers

type 'a t = { values : 'a option Vector.vector }

let unsafe_get t i = (Vector.unsafe_get_array t.values).(i)
let unsafe_set t i v = (Vector.unsafe_get_array t.values).(i) <- v
let create () = { values = Vector.create () }

let get t i =
  if i >= 0 && i < Vector.length t.values then unsafe_get t i else None

let contains t i = get t i |> Option.is_some

let set t i v =
  if i >= Vector.length t.values then
    Vector.resize_with_init t.values ~init:None (i + 1);
  unsafe_set t i (Some v)

let remove t i =
  get t i
  |> Option.map (fun v ->
         unsafe_set t i None;
         v)

let clear t = Vector.clear t.values
