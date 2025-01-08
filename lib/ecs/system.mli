(** Define systems that query and operate on the world. *)

(** An operation that can be performed on the world. *)
type ('world, 'a) operation =
  | Query of ('a -> unit)
  | Immediate of ('world -> 'a -> unit)

type 'world t
(** The type of a system. *)

val make : ('world -> 'a) -> ('world, 'a) operation -> 'world t
(** Make a system from a querier and an operation. *)

val task : ('world, unit) operation -> 'world t
(** Make a system that does not query the world. *)

val run : 'world -> 'world t -> unit
(** Run a system on the world. *)
