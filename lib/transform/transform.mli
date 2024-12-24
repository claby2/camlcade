type t

val identity : unit -> t
val translation : t -> Math.Vec3.t
val rotation : t -> Math.Quat.t
val scale : t -> Math.Vec3.t
val set_translation : t -> Math.Vec3.t -> unit
val set_rotation : t -> Math.Quat.t -> unit
val set_scale : t -> Math.Vec3.t -> unit
val compute_matrix : t -> Math.Mat4.t

module C : Ecs.Component.S with type t = t
