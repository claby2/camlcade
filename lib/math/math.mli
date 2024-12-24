module Vec3 : sig
  include module type of Gg.V3

  val normalize : t -> t
end

module Vec4 : sig
  include module type of Gg.V4

  val normalize : t -> t
end

module Mat3 : sig
  include module type of Gg.M3

  val to_list : t -> float list
end

module Mat4 : sig
  include module type of Gg.M4

  val to_list : t -> float list
end

module Quat : sig
  include module type of Gg.Quat

  val to_axes : t -> Vec4.t * Vec4.t * Vec4.t
end
