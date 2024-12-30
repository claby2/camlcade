type t

val perspective :
  ?near_plane:float ->
  ?far_plane:float ->
  ?aspect_ratio:float ->
  ?height_angle:float ->
  unit ->
  t
(** Construct a new perspective projection. *)

val orthographic : unit -> t
(** Construct a new orthographic projection.

    This is currently unimplemented. *)

val project : t -> Math.Mat4.t
(** Compute the projection matrix. *)

module C : Ecs.Component.S with type t = t
(** Projection component. *)
