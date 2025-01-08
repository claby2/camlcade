(** Make a component for managing events. *)

module type S = sig
  type event
  type t

  val empty : unit -> t
  (** Create an empty event list. *)

  val clear : t -> unit -> unit
  (** Clear all events. *)

  val read : t -> event list
  (** Get a list of all events, from oldest to newest. *)

  val write : t -> event -> unit
  (** Write an event to the event list. *)

  module C : Component.S with type t = t

  val querier : World.t -> t
  (** Querier uses a querier to get a single event component.

      This will panic if there are multiple event components. *)

  val clear_system : World.t System.t
  (** System that clears all events. Should be called after events are read. *)
end

module Make (B : sig
  type t
end) : S with type event = B.t
