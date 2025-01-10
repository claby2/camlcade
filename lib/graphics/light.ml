let default_color = Math.Vec3.v 1. 1. 1.
let default_attenuation = Math.Vec3.v 1. 0. 0.

module Point = struct
  type t = { mutable color : Math.Vec3.t; mutable attenuation : Math.Vec3.t }

  let create ?(color = default_color) ?(attenuation = default_attenuation) () =
    { color; attenuation }

  let color t = t.color
  let attenuation t = t.attenuation
  let set_color t color = t.color <- color
  let set_attenuation t attenuation = t.attenuation <- attenuation

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end

module Directional = struct
  type t = { mutable color : Math.Vec3.t; mutable attenuation : Math.Vec3.t }

  let create ?(color = default_color) ?(attenuation = default_attenuation) () =
    { color; attenuation }

  let color t = t.color
  let attenuation t = t.attenuation
  let set_color t color = t.color <- color
  let set_attenuation t attenuation = t.attenuation <- attenuation

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end

module Spot = struct
  type t = {
    mutable color : Math.Vec3.t;
    mutable attenuation : Math.Vec3.t;
    mutable penumbra : float;
    mutable angle : float;
  }

  let create ?(color = default_color) ?(attenuation = default_attenuation)
      ?(penumbra = 0.1) ?(angle = 0.5) () =
    { color; attenuation; penumbra; angle }

  let color t = t.color
  let attenuation t = t.attenuation
  let penumbra t = t.penumbra
  let angle t = t.angle
  let set_color t color = t.color <- color
  let set_attenuation t attenuation = t.attenuation <- attenuation
  let set_penumbra t penumbra = t.penumbra <- penumbra
  let set_angle t angle = t.angle <- angle

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end
