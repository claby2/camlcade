module type S = sig
  type button
  type t = Up of button | Down of button
end

module Make (T : sig
  type t
end) : S with type button = T.t = struct
  type button = T.t
  type t = Up of button | Down of button
end
