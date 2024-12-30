module Projection = Projection

module Camera3d = struct
  type t = unit

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end
