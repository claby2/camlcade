(** Create a component for managing the state of some buttons. *)

module type S = sig
  type button
  type t

  val create : unit -> t
  (** Create a new keyboard input component. *)

  val is_pressed : t -> button -> bool
  (** Check if a button is currently pressed. *)

  val is_just_pressed : t -> button -> bool
  (** Check if a button was just pressed. *)

  val is_just_released : t -> button -> bool
  (** Check if a button was just released. *)

  module C : Ecs.Component.S with type t = t

  val update_system : Ecs.World.t Ecs.System.t
end

module Make (B : Button.S) (_ : Ecs.Event.S with type event = B.t) :
  S with type button = B.button
