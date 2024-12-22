module Topology : sig
  type t = PointList | LineList | LineStrip | TriangleList
end

module Attribute : sig
  type t

  val size : t -> int
  val values : t -> float array
end

type t

val create : Topology.t -> t
val set_attribute : t -> int -> Attribute.t -> unit
val topology : t -> Topology.t
val attributes : t -> (int, Attribute.t) Hashtbl.t
val vertex_data : t -> float array
