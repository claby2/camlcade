val frag : string
val vert : string

val query :
  Ecs.World.t ->
  (Camera.Projection.t * Transform.t option) list
  * (Mesh3d.t * Transform.t option) list
(** [query q] returns the necessary components for the render function. *)

val render :
  ?transform:Transform.t ->
  ?camera_transform:Transform.t ->
  int ->
  Camera.Projection.t ->
  Mesh3d.t ->
  unit
(** Renders using the normal shader. *)

module C : Ecs.Component.S with type t = unit
(** Normal shader component. *)
