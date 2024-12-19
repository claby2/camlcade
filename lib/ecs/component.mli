type component
type value

module type S = sig
  type t

  val id : Id.Component.t
  val of_component : component -> t
  val to_component : t -> component
end

module Make (Base : sig
  type t
end) : S with type t = Base.t

module Transform : sig
  module T : sig
    type t = Math.Vec3.t
  end

  module C : S with type t = T.t
end

module None : sig
  module T : sig
    type t = unit
  end

  module C : S with type t = T.t
end

val make : 'a. (module S with type t = 'a) -> 'a -> value
val id : value -> Id.Component.t
val extract : value -> component
