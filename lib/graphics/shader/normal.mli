val frag : string
val vert : string

val query :
  (Ecs.Query.t -> Ecs.Query.Result.t) ->
  Camera.Dim3.t list * (Mesh3d.t * Transform.t option) list
(** [query q] returns the necessary components for the render function. *)

val render : ?transform:Transform.t -> int -> Camera.Dim3.t -> Mesh3d.t -> unit

module C : Ecs.Component.S with type t = unit
