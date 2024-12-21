type schedule = Startup | Update
type system = Query.Result.t array -> unit

module Registry : sig
  type t

  val create : t
  val register : t -> schedule -> Query.t array -> system -> unit
  val fetch : t -> schedule -> (Query.t array * system) list
end
