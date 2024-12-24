type base

module type S = sig
  type t

  val id : Id.Component.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make (B : sig
  type inner
end) : S with type t = B.inner

module Transform : sig
  type t = Math.Vec3.t

  module C : S with type t = t
end

module None : sig
  type t = unit

  module C : S with type t = t
end

type packed

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
val unpack : 'a. (module S with type t = 'a) -> packed -> 'a
val unpack_opt : 'a. (module S with type t = 'a) -> packed -> 'a option
val id : packed -> Id.Component.t
