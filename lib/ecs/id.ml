module type S = sig
  type t = int

  val next : unit -> t
  val compare : t -> t -> int
end

module Make () : S = struct
  type t = int

  let current = ref 0

  let next () : t =
    incr current;
    !current

  let compare = compare
end

module Component = Make ()
module Entity = Make ()
module ComponentSet = Set.Make (Component)
module EntitySet = Set.Make (Entity)
