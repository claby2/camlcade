type base = ..

module type S = sig
  type t

  val id : Id.Component.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make (B : sig
  type inner
end) : S with type t = B.inner = struct
  include B

  type t = inner
  type base += T of t

  let id = Id.Component.next ()
  let of_base = function T t -> t | _ -> failwith "Invalid component"
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

module Transform = struct
  type t = Math.Vec3.t

  module C = Make (struct
    type inner = t
  end)
end

(* A component that doesn't have any data.
   Mainly used for optional components in a query result *)
module None = struct
  type t = unit

  module C = Make (struct
    type inner = t
  end)
end

type packed = Packed : (module S with type t = 'a) * 'a -> packed

let pack : type a. (module S with type t = a) -> a -> packed =
 fun component value -> Packed (component, value)

let unpack : packed -> base = function
  | Packed ((module C), value) -> C.to_base value

let id : packed -> Id.Component.t = function Packed ((module C), _) -> C.id
