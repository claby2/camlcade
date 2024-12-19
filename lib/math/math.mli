module Vec3 : sig
  type t

  val make : float -> float -> float -> t
  val zero : t
  val x : t -> float
  val y : t -> float
  val z : t -> float
  val set_x : t -> float -> unit
  val set_y : t -> float -> unit
  val set_z : t -> float -> unit
  val to_string : t -> string
end
