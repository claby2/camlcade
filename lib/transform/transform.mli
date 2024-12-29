type t

(* Constructors *)
val identity : unit -> t
val of_xyz : float -> float -> float -> t

(* Builders *)
val with_translation : Math.Vec3.t -> t -> t
val with_rotation : Math.Quat.t -> t -> t
val with_scale : Math.Vec3.t -> t -> t

(* Getters *)
val translation : t -> Math.Vec3.t
val rotation : t -> Math.Quat.t
val scale : t -> Math.Vec3.t

(* Setters *)
val set_translation : t -> Math.Vec3.t -> unit
val set_rotation : t -> Math.Quat.t -> unit
val set_scale : t -> Math.Vec3.t -> unit

(* Compute *)
val compute_matrix : t -> Math.Mat4.t

module C : Ecs.Component.S with type t = t
