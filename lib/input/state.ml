module type S = sig
  type elt
  type t

  module Set : Set.S with type elt = elt

  val empty : unit -> t
  val press : t -> elt -> unit
  val release : t -> elt -> unit
  val is_pressed : t -> elt -> bool
  val is_just_pressed : t -> elt -> bool
  val is_just_released : t -> elt -> bool

  module C : Ecs.Component.S with type t = t
end

module Make (B : sig
  type t

  val compare : t -> t -> int
end) : S with type elt = B.t = struct
  type elt = B.t

  module Set = Set.Make (B)

  type t = {
    mutable pressed : Set.t;
    mutable just_pressed : Set.t;
    mutable just_released : Set.t;
  }

  let empty () =
    { pressed = Set.empty; just_pressed = Set.empty; just_released = Set.empty }

  let press t key =
    let already_pressed = Set.mem key t.pressed in
    t.pressed <- Set.add key t.pressed;
    t.just_pressed <-
      (if already_pressed then t.just_pressed else Set.add key t.just_pressed)

  let release t key =
    let already_pressed = Set.mem key t.pressed in
    t.pressed <- Set.remove key t.pressed;
    t.just_pressed <- Set.remove key t.just_pressed;
    t.just_released <-
      (if already_pressed then Set.add key t.just_released else t.just_released)

  let is_pressed t key = Set.mem key t.pressed
  let is_just_pressed t key = Set.mem key t.just_pressed
  let is_just_released t key = Set.mem key t.just_released

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end

module Keys = Make (Key)

module Window_events = struct
  type t = Window.t list ref

  let empty () = ref []
  let clear t = t := []
  let push t e = t := e :: !t
  let iter t f = !t |> List.iter f

  module C = Ecs.Component.Make (struct
    type inner = t
  end)
end
