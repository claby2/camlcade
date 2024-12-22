module T : sig
  type t

  val empty : unit -> t
  val initialize : gl:int * int -> t -> unit
  val render : t -> unit
  val destroy : t -> unit
end

module C : Ecs.Component.S with type t = T.t
