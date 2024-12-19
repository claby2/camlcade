module Hash : sig
  type t = int

  val hash : 'a list -> t
  val compare : t -> t -> int
end

module Edges : sig
  type ('a, 'b) t

  val empty : unit -> ('a, 'b) t
  val find_add_opt : ('a, 'b) t -> 'a -> 'b option
  val find_remove_opt : ('a, 'b) t -> 'a -> 'b option
  val replace_add : ('a, 'b) t -> 'a -> 'b option -> unit
  val replace_remove : ('a, 'b) t -> 'a -> 'b option -> unit
end

type t

val empty : unit -> t
val create : Id.ComponentSet.t -> t
val to_string : t -> string
val hash : t -> Hash.t
val components : t -> Id.ComponentSet.t
val edges : t -> (Id.Component.t, Hash.t) Edges.t
val entities : t -> Id.EntitySet.t
val extract_entity : t -> Id.Entity.t -> Component.value list
val add_entity : t -> Id.Entity.t -> Component.value list -> unit
val get_component : t -> Id.Component.t -> Id.Entity.t -> Component.value option

exception Entity_not_found
exception Invalid_components
