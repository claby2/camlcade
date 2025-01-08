type t

val create :
  ?ambient:Math.Vec3.t ->
  ?diffuse:Math.Vec3.t ->
  ?specular:Math.Vec3.t ->
  ?shininess:float ->
  unit ->
  t

val ambient : t -> Math.Vec3.t
val diffuse : t -> Math.Vec3.t
val specular : t -> Math.Vec3.t
val shininess : t -> float
val set_shininess : t -> float -> unit

module C : Ecs.Component.S with type t = t
