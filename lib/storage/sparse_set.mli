type 'a t

val create : unit -> 'a t
val length : 'a t -> int
val contains : 'a t -> int -> bool
val get : 'a t -> int -> 'a option
val set : 'a t -> int -> 'a -> unit
val remove : 'a t -> int -> 'a option
