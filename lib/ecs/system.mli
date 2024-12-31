type querier = Query.t -> Query.Result.t

type ('world, 'a) operation =
  | Query of ('a -> unit)
  | Immediate of ('world -> 'a -> unit)

type ('world, 'a) t'
type 'world t = System : ('world, 'a) t' -> 'world t

val make : (querier -> 'a) -> ('world, 'a) operation -> 'world t
val task : ('world, unit) operation -> 'world t
val run : 'world -> querier -> 'world t -> unit
