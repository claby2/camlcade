open Math

type component = ..

module type S = sig
  type t

  val id : Id.Component.t
  val of_component : component -> t
  val to_component : t -> component
end

module Make (Base : sig
  type t
end) : S with type t = Base.t = struct
  include Base

  type component += T of t

  let id = Id.Component.next ()
  let of_component = function T t -> t | _ -> failwith "Invalid component"
  let to_component t = T t
end

module Transform = struct
  module T = struct
    type t = Vec3.t
  end

  module C = Make (T)
end

(* TODO: Should probably get rid of this Health component *)
module Health = struct
  module T = struct
    type t = int
  end

  module C = Make (T)
end

(* A component that doesn't have any data.
   Mainly used for optional components in a query result *)
module None = struct
  module T = struct
    type t = unit
  end

  module C = Make (T)
end

type value = Value : (module S with type t = 'a) * 'a -> value

let make : type a. (module S with type t = a) -> a -> value =
 fun component value -> Value (component, value)

let id : value -> Id.Component.t = function Value ((module C), _) -> C.id

let extract : value -> component = function
  | Value ((module C), value) -> C.to_component value
