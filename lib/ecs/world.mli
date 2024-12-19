type t

(**/**)

val to_string : t -> string
val evaluate_query : t -> Query.t -> Query.Result.t

(**/**)

val create : unit -> t
val add_entity : t -> Id.Entity.t
val remove_entity : t -> Id.Entity.t -> unit
val add_component : t -> Component.packed -> Id.Entity.t -> unit
val remove_component : t -> Id.Component.t -> Id.Entity.t -> unit

val get_component :
  t -> Id.Component.t -> Id.Entity.t -> Component.packed option

val add_system : t -> System.schedule -> Query.t -> System.system -> unit
val run_systems : t -> System.schedule -> unit
