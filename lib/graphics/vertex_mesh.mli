module Attribute : sig
  type info = { index : int; size : int }
  type t

  val create : index:int -> size:int -> float array -> t
  val index : t -> int
  val size : t -> int
  val info : t -> info
  val values : t -> float array
  val compare : t -> t -> int
end

module AttributeSet : Set.S with type elt = Attribute.t

type topology = TriangleList
type t

val create : topology -> t
(** [create topology] creates a new mesh with the given topology *)

val topology : t -> topology
(** [topology t] returns the topology of the mesh *)

val with_topology : topology -> t -> t
(** [with_topology t topology] sets the topology of the mesh *)

val with_attribute : Attribute.t -> t -> t
(** [with_attribute t attr] adds the attribute to the mesh *)

val of_primitive : Primitive.t -> t
(** [of_primitive primitive] creates a mesh from the given primitive *)

val count_vertices : t -> int
(** [count_vertices t] returns the minimum number of vertices reported by the
    attributes *)

val vertex_size : t -> int
(** [vertex_size t] returns the size of a vertex in the mesh *)

val attribute_info : t -> Attribute.info Seq.t
(** Returns the a sequence of attribute info ordered by index *)

val vertex_data : t -> float array
(** [vertex_data t] returns the interleaved vertex data *)
