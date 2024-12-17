module Make () = struct
  type t = int

  let current = ref 0
  let compare = compare

  let next () : t =
    current := !current + 1;
    !current
end

module Component = Make ()
module Entity = Make ()
