module type S = sig
  type elt
  type t

  module Set : Set.S with type elt = elt

  val empty : unit -> t
  val press : t -> elt -> unit
  val release : t -> elt -> unit
  val is_pressed : t -> elt -> bool
  val is_just_pressed : t -> elt -> bool
  val is_just_released : t -> elt -> bool

  module C : Ecs.Component.S with type t = t
end

module Keys : S with type elt = Key.t

module Window_events : sig
  type t

  val empty : unit -> t
  val clear : t -> unit
  val push : t -> Window.t -> unit
  val iter : t -> (Window.t -> unit) -> unit

  module C : Ecs.Component.S with type t = t
end
