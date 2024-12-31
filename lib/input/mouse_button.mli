(** Mouse button events. *)

type mouse_button = [ `Left | `Middle | `Right | `X1 | `X2 | `Unknown of int ]

val of_int : int -> mouse_button

include Button.S with type button = mouse_button
