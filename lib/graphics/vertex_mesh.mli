(** Vertex mesh with attributes. *)

type topology = TriangleList
type t

val create : ?topology:topology -> unit -> t
(** Creates a new vertex mesh. *)

val topology : t -> topology
(** [topology t] returns the topology of the vertex mesh. *)

val set_topology : t -> topology -> unit
(** [set_topology t topology] sets the topology of the vertex mesh. *)

val attributes : t -> (int, int) Hashtbl.t
(** [attributes t] returns the attributes of the vertex mesh. *)

val set_attribute : t -> int -> int -> unit
(** [set_attribute t index size] sets the attribute of the vertex mesh. *)

val data : t -> float array
(** [data t] returns the data of the vertex mesh. *)

val set_data : t -> float array -> unit
(** [set_data t data] sets the data of the vertex mesh. *)

val vertex_size : t -> int
(** [vertex_size t] returns the size of the vertex in the vertex mesh. *)

val count_vertices : t -> int
(** [count_vertices t] returns the number of vertices in the vertex mesh. *)
