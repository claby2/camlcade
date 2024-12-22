module Vec3 : sig
  type t

  val make : float -> float -> float -> t
  val zero : t
  val xyz : t -> float * float * float
  val x : t -> float
  val y : t -> float
  val z : t -> float
  val set_x : t -> float -> unit
  val set_y : t -> float -> unit
  val set_z : t -> float -> unit
  val to_string : t -> string
  val sum : t -> float
  val map : (float -> float) -> t -> t
  val map2 : (float -> float -> float) -> t -> t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val dot : t -> t -> float
  val length : t -> float
  val length_squared : t -> float
  val length_recip : t -> float
  val normalize : t -> t
  val neg : t -> t
  val scale : float -> t -> t
  val cross : t -> t -> t
end

module type Matrix = sig
  type t

  val create : ?default:float -> unit -> t
  val identity : t
  val from_array : float array array -> t
  val to_array : t -> float array array
  val inverse : t -> t
  val transpose : t -> t
  val ( * ) : t -> t -> t
end

module Mat3 : Matrix
module Mat4 : Matrix
