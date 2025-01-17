(** A first person camera plugin.

    This plugin allows the user to move the camera using the keyboard and the
    mouse. To use this plugin, add [Fp_camera.C] to the camera entity and add
    the [Fp_camera.plugin] to the app. *)

module C : Ecs.Component.S with type t = unit

val plugin :
  ?mouse_sensitivity:float ->
  ?move_factor:float ->
  ?fullscreen:bool ->
  Ecs.World.t ->
  unit
