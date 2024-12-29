module Filter : sig
  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Wildcard

  val matches : t -> Id.ComponentSet.t -> bool
end

module Result : sig
  type t = (Id.Entity.t * Component.packed list) list

  val entity_single : t -> (Id.Entity.t * Component.packed list) option
  val single : t -> Component.packed list option
  val entity_iter : (Id.Entity.t * Component.packed list -> unit) -> t -> unit
  val iter : (Component.packed list -> unit) -> t -> unit
  val entity_map : (Id.Entity.t * Component.packed list -> 'a) -> t -> 'a list
  val map : (Component.packed list -> 'a) -> t -> 'a list
  val entity_filter : (Id.Entity.t * Component.packed list -> bool) -> t -> t
  val filter : (Component.packed list -> bool) -> t -> t

  val entity_filter_map :
    (Id.Entity.t * Component.packed list -> 'a option) -> t -> 'a list

  val filter_map : (Component.packed list -> 'a option) -> t -> 'a list
end

type term = Required of Id.Component.t | Optional of Id.Component.t
type t

val create : ?filter:Filter.t -> term list -> t
val required_components : t -> Id.Component.t list
val evaluate : t -> Archetype.t list -> Result.t
