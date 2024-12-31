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
  type entity = Id.Entity.t * Component.packed list
  type t = entity list

  let entity_single r =
    match r with
    | [ (entity, components) ] -> Some (entity, components)
    | _ -> None

  let single r =
    match entity_single r with
    | Some (_, components) -> Some components
    | None -> None

  let entity_iter = List.iter
  let iter f = List.iter (fun (_, components) -> f components)
  let entity_map = List.map
  let map f = List.map (fun (_, components) -> f components)
  let entity_filter = List.filter
  let filter f = List.filter (fun (_, components) -> f components)
  let entity_filter_map = List.filter_map
  let filter_map f = List.filter_map (fun (_, components) -> f components)

  let as_list (type a) (module C : Component.S with type t = a) (r : t) : a list
      =
    map
      (function
        | [ c ] -> Component.unpack (module C) c
        | _ -> invalid_arg "Expected exactly one component in each entity")
      r

  let as_single (type a) (module C : Component.S with type t = a) (r : t) :
      a option =
    match single r with
    | Some [ c ] -> Some (Component.unpack (module C) c)
    | _ -> None
end

type term = Required of Id.Component.t | Optional of Id.Component.t
type t = { terms : term list; filter : Filter.t }

let create ?(filter = Filter.Wildcard) terms = { terms; filter }

let required_components q =
  q.terms |> List.filter_map (function Required c -> Some c | _ -> None)

let evaluate q archetypes =
  if List.is_empty q.terms then []
  else
    let matches_filter a = Filter.matches q.filter (Archetype.components a) in

    let fetch_components a e =
      let get_component = Archetype.get_component a in
      List.map
        (function
          | Required c -> get_component e c |> Option.get
          | Optional c ->
              get_component e c
              |> Option.value
                   ~default:(Component.pack (module Component.None.C) ()))
        q.terms
    in

    let build_result a =
      Archetype.entities a |> Id.EntitySet.to_list
      |> List.map (fun e -> (e, fetch_components a e))
    in

    archetypes |> List.filter matches_filter |> List.concat_map build_result
