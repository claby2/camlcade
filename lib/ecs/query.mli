(** Construct and evaluate queries. *)

(** A filter that can be used to filter entities based on their components. *)
module Filter : sig
  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Wildcard

  val matches : t -> Id.ComponentSet.t -> bool
  (** Returns true if the given component set matches the filter. *)
end

(** A query term that can be used to construct queries. *)
type 'a term =
  | Req : (module Component.S with type t = 'a) -> 'a term
      (** A required component must be present in an entity. *)
  | Opt : (module Component.S with type t = 'a) -> 'a option term
      (** An optional component will be None if an entity does not have it. *)

(** The type of a query. *)
type 'a t = [] : unit t | ( :: ) : 'a term * 'b t -> ('a * 'b) t

val required_ids : 'a t -> Id.ComponentSet.t
(** Returns the set of required component IDs for the given query. *)

val evaluate :
  ?filter:Filter.t -> 'a t -> Archetype.t list -> (Id.Entity.t * 'a) list
(** Evaluates the given query on the given archetypes. *)
