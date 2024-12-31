(** A button is a type that can be in an up or down state.

    This is intentionally made as generic as possible to allow for different
    types of buttons. *)

module type S = sig
  type button
  type t = Up of button | Down of button
end

module Make (T : sig
  type t
end) : S with type button = T.t
