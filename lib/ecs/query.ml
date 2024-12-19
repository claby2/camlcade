module Filter = struct
  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Wildcard

  (* Returns true if the given component set matches the filter *)
  let matches f components =
    let rec aux f components =
      match f with
      | With c -> Id.ComponentSet.mem c components
      | Without c -> not (Id.ComponentSet.mem c components)
      | Not f -> not (aux f components)
      | And (f1, f2) -> aux f1 components && aux f2 components
      | Or (f1, f2) -> aux f1 components || aux f2 components
      | Wildcard -> true
    in
    aux f components
end

type term = Required of Id.Component.t | Optional of Id.Component.t
type t = { terms : term list; filter : Filter.t }

let create ?(filter = Filter.Wildcard) terms = { terms; filter }
let terms q = q.terms
let filter q = q.filter

module Result = struct
  type t = (Id.Entity.t * Component.value list) list
end

(* SCRATCH:

Queries one would want to express:

[Transform.C.id, Health.C.id] -> [[t1, h1], [t2, h2], ...]
- Query all components with both Transform and Health components

[Optional Transform.C.id, Health.C.id] -> [[t1, h1], [n, h1], ...]
- Query all components with health and might have a transform
  If transform is not present, then a custom None component will be returned in-place

[AnyOf [Transform.C.id, Health.C.id], Other.C.id] ->
    [[t1, h1, o1], [n, h2, o2], [t3, n, o3], ...]
- Query all components that have one of Transform or Health (can be both) and Other
*)
