type querier = Query.t -> Query.Result.t

type ('world, 'a) operation =
  | Query of ('a -> unit)
  | Immediate of ('world -> 'a -> unit)

type ('world, 'a) t' = {
  querier : querier -> 'a;
  operation : ('world, 'a) operation;
}

type 'world t = System : ('world, 'a) t' -> 'world t

let make querier operation = System { querier; operation }

let run world querier (System { querier = q; operation = op }) =
  match op with Query f -> f (q querier) | Immediate f -> f world (q querier)
