type t

val create : unit -> t
val add_entity : t -> Id.Entity.t
val remove_entity : t -> Id.Entity.t -> unit
val add_component : t -> Component.packed -> Id.Entity.t -> unit
val with_component : t -> Component.packed -> Id.Entity.t -> Id.Entity.t
val remove_component : t -> Id.Component.t -> Id.Entity.t -> unit

val get_component :
  t -> Id.Component.t -> Id.Entity.t -> Component.packed option

val add_system : t -> Scheduler.schedule -> Query.t array -> t System.t -> unit
val run_systems : t -> Scheduler.schedule -> unit

(**/**)

val evaluate_query : t -> Query.t -> Query.Result.t

(**/**)
