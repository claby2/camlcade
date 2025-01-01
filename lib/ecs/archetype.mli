(** Archetype storage and manipulation. *)

(** Indicate an operation to add or remove a component. *)
type operation = Add of Id.Component.t | Remove of Id.Component.t

type t

val create : Id.ComponentSet.t -> t
(** Creates a new archetype with the given components. *)

val empty : unit -> t
(** Creates an archetype with no components. *)

val hash : t -> int
(** Returns the hash of the archetype. *)

val components : t -> Id.ComponentSet.t
(** Returns the components of the archetype. *)

val entities : t -> Id.EntitySet.t
(** Returns the entities in the archetype. *)

val query : t -> Id.Entity.t -> Id.Component.t -> Component.packed option
(** Queries the archetype for an entity's component. If the entity does not have
    the component, returns [None]. *)

val remove : t -> Id.Entity.t -> unit
(** Removes an entity from the archetype. No change if the entity is not in the
    archetype. *)

val add : t -> Id.Entity.t -> Component.packed list -> unit
(** Adds an entity to the archetype with the given components. The components
    must match the archetype's components exactly. *)

val replace : t -> Id.Entity.t -> Component.packed -> unit
(** Replaces an entity's component in the archetype. The entity must already be
    in the archetype and the component be one of the archetype's components. *)

val next_hash : t -> operation -> int
(** Returns the hash of the archetype after the given operation. Caches results
    for future calls. *)
