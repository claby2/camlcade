type schedule = Startup | Update | Last
type 'a t

val create : unit -> 'a t
val register : 'a t -> schedule -> 'a -> unit
val fetch : 'a t -> schedule -> 'a list
