module type S = sig
  type t = int

  val next : unit -> t
  val compare : t -> t -> int
end

module Make () : S
module Component : S
module Entity : S
module ComponentSet : Set.S with type elt = Component.t
module EntitySet : Set.S with type elt = Entity.t
