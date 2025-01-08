type ('world, 'a) operation =
  | Query of ('a -> unit)
  | Immediate of ('world -> 'a -> unit)

type ('world, 'a) t' = {
  querier : 'world -> 'a;
  operation : ('world, 'a) operation;
}

type 'world t = System : ('world, 'a) t' -> 'world t

let make querier operation = System { querier; operation }
let task operation = System { querier = (fun _ -> ()); operation }

let run world (System { querier = q; operation = op }) =
  match op with Query f -> f (q world) | Immediate f -> f world (q world)
