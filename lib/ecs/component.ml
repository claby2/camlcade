open Math

type component = ..

module type Base = sig
  type t

  val of_component : component -> t
  val to_component : t -> component
end

module type T = sig
  include Base

  val id : Id.Component.t
end

module Make (Base : Base) : T with type t = Base.t = struct
  include Base

  let id = Id.Component.next ()
end

module Transform = Make (struct
  type t = Vec3.t
  type component += Transform of t

  let of_component = function
    | Transform t -> t
    | _ -> failwith "Invalid component"

  let to_component t = Transform t
end)

module Health = Make (struct
  type t = int
  type component += Health of t

  let of_component = function
    | Health t -> t
    | _ -> failwith "Invalid component"

  let to_component t = Health t
end)

type value = Value : (module T with type t = 'a) * 'a -> value

let make : type a. (module T with type t = a) -> a -> value =
 fun component value -> Value (component, value)

let id : value -> Id.Component.t = function Value ((module C), _) -> C.id
