open Ecs

module Foo = struct
  type t = int ref

  module C = Component.Make (struct
    type inner = t
  end)
end

module Bar = struct
  type t = int ref

  module C = Component.Make (struct
    type inner = t
  end)
end

module Baz = struct
  type t = int ref

  module C = Component.Make (struct
    type inner = t
  end)
end

module Name = struct
  type t = string ref

  module C = Component.Make (struct
    type inner = t
  end)
end
