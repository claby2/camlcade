type program

val create : frag:string -> vert:string -> program
val pid : program -> int
val destroy : program -> unit

module Manager : sig
  type t

  val empty : unit -> t
  val initialize : t -> unit
  val with_phong : t -> Math.Mat4.t -> Math.Mat4.t -> (int -> unit) -> unit
  val destroy : t -> unit
end
