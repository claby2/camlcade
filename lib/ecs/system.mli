type 'world t =
  | Query of (Query.Result.t array -> unit)
  | Immediate of ('world -> Query.Result.t array -> unit)
