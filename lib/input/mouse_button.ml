type mouse_button = [ `Left | `Middle | `Right | `X1 | `X2 | `Unknown of int ]

let of_int = function
  | 1 -> `Left
  | 2 -> `Right
  | 3 -> `Middle
  | 4 -> `X1
  | 5 -> `X2
  | i -> `Unknown i

include Button.Make (struct
  type t = mouse_button
end)
