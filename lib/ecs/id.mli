(** Incremental identifiers. *)

(** A module type for incremental identifiers. *)
module type S = sig
  type t

  val next : unit -> t
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
end

module Component : S
(** Incremental identifier for components. *)

module Entity : S
(** Incremental identifier for entities. *)

module ComponentSet : Set.S with type elt = Component.t
(** Set of component identifiers. *)

module EntitySet : Set.S with type elt = Entity.t
(** Set of entity identifiers. *)
