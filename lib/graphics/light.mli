(** Light components. *)

(** Point light.

    Use {!Transform.C} component to position the light. Direction has no effect.
*)
module Point : sig
  type t

  val create : ?color:Math.Vec3.t -> ?attenuation:Math.Vec3.t -> unit -> t
  val color : t -> Math.Vec3.t
  val attenuation : t -> Math.Vec3.t
  val set_color : t -> Math.Vec3.t -> unit
  val set_attenuation : t -> Math.Vec3.t -> unit

  module C : Ecs.Component.S with type t = t
end

(** Directional light.

    Use {!Transform.C} component to direct the light. Position has no effect. *)
module Directional : sig
  type t

  val create : ?color:Math.Vec3.t -> ?attenuation:Math.Vec3.t -> unit -> t
  val color : t -> Math.Vec3.t
  val attenuation : t -> Math.Vec3.t
  val set_color : t -> Math.Vec3.t -> unit
  val set_attenuation : t -> Math.Vec3.t -> unit

  module C : Ecs.Component.S with type t = t
end

(** Spot light.

    Use {!Transform.C} component to position and direct the light. *)
module Spot : sig
  type t

  val create :
    ?color:Math.Vec3.t ->
    ?attenuation:Math.Vec3.t ->
    ?penumbra:float ->
    ?angle:float ->
    unit ->
    t

  val color : t -> Math.Vec3.t
  val attenuation : t -> Math.Vec3.t
  val penumbra : t -> float
  val angle : t -> float
  val set_penumbra : t -> float -> unit
  val set_angle : t -> float -> unit
  val set_color : t -> Math.Vec3.t -> unit
  val set_attenuation : t -> Math.Vec3.t -> unit

  module C : Ecs.Component.S with type t = t
end
