module type S = sig
  type event
  type t

  val empty : unit -> t
  val clear : t -> unit -> unit
  val read : t -> event list
  val write : t -> event -> unit

  module C : Component.S with type t = t

  val querier : World.t -> t
  val clear_system : World.t System.t
end

module Make (B : sig
  type t
end) : S with type event = B.t = struct
  type event = B.t
  type t = event list ref

  let empty () = ref []
  let clear t () = t := []
  let read t = List.rev !t
  let write t e = t := e :: !t

  module C = Component.Make (struct
    type inner = t
  end)

  let querier w =
    let _, (t, ()) = World.query w Query.(Req (module C) @ Nil) |> List.hd in
    t

  let clear_system =
    let clear t = clear t () in
    System.make querier (System.Query clear)
end
