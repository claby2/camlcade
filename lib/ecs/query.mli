(** Query entities and their components. *)

(** {1:filters Filters} *)

(** Specify which components should be or should not be present in the entities
    that match the query. *)
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

(** {1:results Results} *)

(** The result of a query. *)
module Result : sig
  type entity = Id.Entity.t * Component.packed list
  type t = entity list

  val entity_single : t -> entity option
  (** Returns the single entity in the result, if there is exactly one. *)

  val single : t -> Component.packed list option
  (** Returns the single entity's components, if there is exactly one. *)

  val entity_iter : (entity -> unit) -> t -> unit
  (** Iterate over the entities in the result. *)

  val iter : (Component.packed list -> unit) -> t -> unit
  (** Iterate over the components of the entities in the result. *)

  val entity_map : (entity -> 'a) -> t -> 'a list
  (** Map over the entities in the result. *)

  val map : (Component.packed list -> 'a) -> t -> 'a list
  (** Map over the components of the entities in the result. *)

  val entity_filter : (entity -> bool) -> t -> t
  (** Filter the entities in the result. *)

  val filter : (Component.packed list -> bool) -> t -> t
  (** Filter the components of the entities in the result. *)

  val entity_filter_map :
    (Id.Entity.t * Component.packed list -> 'a option) -> t -> 'a list
  (** Filter-map over the entities in the result. *)

  val filter_map : (Component.packed list -> 'a option) -> t -> 'a list
  (** Filter-map over the components of the entities in the result. *)

  val as_list : (module Component.S with type t = 'a) -> t -> 'a list
  (** Get the components of the entities in the result. Will raise an exception
      if there is not exactly one component of the desired type in each entity.
  *)

  val as_single : (module Component.S with type t = 'a) -> t -> 'a option
  (** Get the single component of the single entity in the result. *)
end

(** {1:queries Queries} *)

type term = Required of Id.Component.t | Optional of Id.Component.t
type t

val create : ?filter:Filter.t -> term list -> t
(** Create a query. *)

val required_components : t -> Id.Component.t list
(** Get the components that are required by the query. *)

val evaluate : t -> Archetype.t list -> Result.t
(** Evaluate the query on a list of archetypes.

    Each archetype should have the required components of the query. *)
