type 'a t

val create : unit -> 'a t
val get : 'a t -> int -> 'a option
val contains : 'a t -> int -> bool
val set : 'a t -> int -> 'a -> unit
val remove : 'a t -> int -> 'a option
val clear : 'a t -> unit
