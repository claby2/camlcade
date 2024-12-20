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

module Result = struct
  type t = (Id.Entity.t * Component.packed list) list
end

type term = Required of Id.Component.t | Optional of Id.Component.t
type t = { terms : term list; filter : Filter.t }

let create ?(filter = Filter.Wildcard) terms = { terms; filter }

let required_components q =
  q.terms |> List.filter_map (function Required c -> Some c | _ -> None)

let evaluate q archetypes =
  archetypes
  (* Only consider archetypes that match the filter *)
  |> List.filter (fun a -> Filter.matches q.filter (Archetype.components a))
  |> List.map (fun a ->
         let entities = Archetype.entities a |> Id.EntitySet.to_list in
         entities
         |> List.map (fun e ->
                let response_components =
                  q.terms
                  |> List.map (function
                       | Required c ->
                           Archetype.get_component a c e |> Option.get
                       | Optional c ->
                           (* If the component is not present, return a None component *)
                           Archetype.get_component a c e
                           |> Option.value
                                ~default:
                                  (Component.pack (module Component.None.C) ()))
                in
                (e, response_components)))
  |> List.concat

(* SCRATCH:

A : component_id -> archetype_id list
if all optional, then the search space is ALL archetypes
else, intersect all the required archetypes


[c1, c2, optionally c3]

A(c1) \cap A(c2)

[optionally c1, optionally c2]

ALL

[optionally c1, c2, optionally c3]

A(c2)

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
