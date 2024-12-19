type base

module type S = sig
  type t

  val id : Id.Component.t
  val of_base : base -> t
  val to_base : t -> base
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

type packed

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
val unpack : packed -> base
val id : packed -> Id.Component.t
