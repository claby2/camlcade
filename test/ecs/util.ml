open Ecs

module Foo = struct
  module T = struct
    type t = int ref
  end

  module C = Component.Make (T)
end

module Bar = struct
  module T = struct
    type t = int ref
  end

  module C = Component.Make (T)
end

module Baz = struct
  module T = struct
    type t = int ref
  end

  module C = Component.Make (T)
end

module Name = struct
  module T = struct
    type t = string ref
  end

  module C = Component.Make (T)
end
