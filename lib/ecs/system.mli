type schedule = Startup | Update
type system = Query.Result.t -> unit

module Registry : sig
  type t

  val create : t
  val register : t -> schedule -> Query.t -> system -> unit
  val fetch : t -> schedule -> (Query.t * system) list
end
