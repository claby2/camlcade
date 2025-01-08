val bigarray_create :
  ('a, 'b) Bigarray.kind -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

val get_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit) ->
  int

val set_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit) ->
  int ->
  unit

val get_string :
  int ->
  ((char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit) ->
  string

val ( >>= ) : ('a, [< `Msg of string ]) result -> ('a -> 'b) -> 'b
val load_matrix4fv : Math.Mat4.t -> int -> string -> unit
val load_matrix3fv : Math.Mat3.t -> int -> string -> unit
val load_uniform1i : int -> int -> string -> unit
val load_uniform1f : float -> int -> string -> unit
val load_uniform3fv : Math.Vec3.t -> int -> string -> unit
val check_gl_error : unit -> unit
val add_vec3 : float list ref -> Math.Vec3.t -> unit
