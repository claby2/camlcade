module Projection = Projection

module Camera3d : sig
  type t = unit

  module C : Ecs.Component.S with type t = t
end
