(*
module Dim3 : sig
  module T : sig
    type t

    val create :
      ?look:Math.Vec3.t ->
      ?up:Math.Vec3.t ->
      ?height_angle:float ->
      ?near_plane:float ->
      ?far_plane:float ->
      ?aspect_ratio:float ->
      Math.Vec3.t ->
      t

    val view : t -> unit
    val projection : t -> unit
    val look : t -> Math.Vec3.t
    val up : t -> Math.Vec3.t
    val set_pos : t -> Math.Vec3.t -> unit
    val move_pos : t -> Math.Vec3.t -> unit
    val rotate : t -> float -> Math.Vec3.t -> unit
    val set_aspect_ratio : t -> float -> unit
    val set_near_plane : t -> float -> unit
    val set_far_plane : t -> float -> unit
  end

  module C : Ecs.Component.S with type t = T.t
end
*)
