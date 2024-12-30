(** Represents translation, rotation, and scale. *)

type t
(** The type of a transform. *)

(** {1:transforms Transforms} *)

val identity : unit -> t
(** Identity transform. *)

val of_xyz : float -> float -> float -> t
(** Create a transform with the given translation. *)

(** {1:builders Builders} *)

val with_translation : Math.Vec3.t -> t -> t
(** Build a transform with the given translation. *)

val with_rotation : Math.Quat.t -> t -> t
(** Build a transform with the given rotation. *)

val with_scale : Math.Vec3.t -> t -> t
(** Build a transform with the given scale. *)

val with_look_to : ?up:Math.Vec3.t -> Math.Vec3.t -> t -> t
(** [with_look_to ~up dir t] returns a new transform that looks in the direction
    [dir] with the up vector [up]. *)

val with_look_at : ?up:Math.Vec3.t -> Math.Vec3.t -> t -> t
(** [with_look_at ~up target t] returns a new transform that looks at the target
    [target] with the up vector [up]. *)

(** {1:operations Operations} *)

(** {1:getters Getters} *)

val translation : t -> Math.Vec3.t
(** Return the translation of the transform. *)

val rotation : t -> Math.Quat.t
(** Return the rotation of the transform. *)

val scale : t -> Math.Vec3.t
(** Return the scale of the transform. *)

(** {1:setters Setters} *)

val set_translation : t -> Math.Vec3.t -> unit
(** Set the translation of the transform. *)

val set_rotation : t -> Math.Quat.t -> unit
(** Set the rotation of the transform. *)

val set_scale : t -> Math.Vec3.t -> unit
(** Set the scale of the transform. *)

(** {1:compute Compute} *)

val compute_matrix : t -> Math.Mat4.t
(** Calculate the transformation matrix from the translation, rotation, and
    scale. *)

(** {1:component Component} *)

module C : Ecs.Component.S with type t = t
(** Transform component *)
