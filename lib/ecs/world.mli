type t

val create : unit -> t

(* Entities *)
val add_entity : t -> Id.Entity.t
val remove_entity : t -> Id.Entity.t -> unit
val entities : t -> Id.Entity.t list

(* Components *)
val add_component : t -> Component.packed -> Id.Entity.t -> unit

val with_component :
  'a.
  t -> (module Component.S with type t = 'a) -> 'a -> Id.Entity.t -> Id.Entity.t

val remove_component : t -> Id.Component.t -> Id.Entity.t -> unit

val get_component :
  t -> Id.Entity.t -> Id.Component.t -> Component.packed option

(* Systems *)
val query : ?filter:Query.Filter.t -> t -> 'a Query.t -> (Id.Entity.t * 'a) list
val add_system : t -> Scheduler.schedule -> t System.t -> unit
val run_systems : t -> Scheduler.schedule -> unit

exception Quit

val has_quit : t -> bool
