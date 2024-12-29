type ('world, 'a) operation =
  | Query of ('a -> unit)
  | Immediate of ('world -> 'a -> unit)

type ('world, 'a) t'
type 'world t = System : ('world, 'a) t' -> 'world t

val make :
  ((Query.t -> Query.Result.t) -> 'a) -> ('world, 'a) operation -> 'world t

val run : 'world -> (Query.t -> Query.Result.t) -> 'world t -> unit
