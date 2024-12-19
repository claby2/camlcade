module ArchetypeHashSet = Set.Make (Archetype.Hash)

type t = {
  archetype_index : (Archetype.Hash.t, Archetype.t) Hashtbl.t;
  (* TODO: entity id -> (archetype, row) *)
  entity_index : (Id.Entity.t, Archetype.Hash.t) Hashtbl.t;
  (* TODO: component id -> (archetype id -> column) *)
  component_index : (Id.Component.t, ArchetypeHashSet.t) Hashtbl.t;
  systems : System.Registry.t;
}

let to_string w =
  Printf.sprintf
    "World {\narchetype_index = %s\nentity_index = %s\ncomponent_index = %s\n}"
    (Hashtbl.fold
       (fun k v acc ->
         Printf.sprintf "%s%d -> %s\n" acc k (Archetype.to_string v))
       w.archetype_index "\n")
    (Hashtbl.fold
       (fun k v acc -> Printf.sprintf "%s%d -> %d\n" acc k v)
       w.entity_index "\n")
    (Hashtbl.fold
       (fun k v acc ->
         Printf.sprintf "%s%d -> %s}\n" acc k
           (ArchetypeHashSet.elements v
           |> List.fold_left (fun acc h -> Printf.sprintf "%s%d, " acc h) "{"))
       w.component_index "\n")

(* Create a new empty world *)
let empty =
  (* Start with the empty archetype in the archetype index *)
  let archetype_index = Hashtbl.create 0 in
  let empty_archetype = Archetype.empty in
  Hashtbl.add archetype_index empty_archetype.hash empty_archetype;
  {
    archetype_index;
    entity_index = Hashtbl.create 0;
    component_index = Hashtbl.create 0;
    systems = System.Registry.create;
  }

let get_archetype w entity =
  Hashtbl.find w.archetype_index (Hashtbl.find w.entity_index entity)

(* Add a new entity and return its id *)
let add_entity w =
  let entity = Id.Entity.next () in
  Hashtbl.add w.entity_index entity (Archetype.hash Archetype.empty);
  entity

(* Get the value of a specific component for a given entity if it exists *)
let get_component w component entity =
  let archetype = get_archetype w entity in
  Archetype.get_component archetype component entity

(* Add a component to an entity *)
let add_component w component entity =
  let archetype = get_archetype w entity in
  let archetype_edges = Archetype.edges archetype in
  let new_archetype =
    match
      Archetype.Edges.find_add_opt archetype_edges (Component.id component)
    with
    | Some hash -> Hashtbl.find w.archetype_index hash
    | None ->
        let new_archetype =
          Archetype.create
            (Component.id component :: Archetype.components archetype)
        in
        Archetype.Edges.replace_add archetype_edges (Component.id component)
          (Some new_archetype.hash);
        new_archetype
  in
  (* Move entity from old archetype to new archetype *)
  Archetype.add_entity new_archetype entity
    (Archetype.extract_entity archetype entity @ [ component ]);
  Hashtbl.replace w.archetype_index new_archetype.hash new_archetype;
  Hashtbl.replace w.entity_index entity new_archetype.hash;
  let new_archetype_set =
    match Hashtbl.find_opt w.component_index (Component.id component) with
    | None -> ArchetypeHashSet.singleton new_archetype.hash
    | Some set -> ArchetypeHashSet.add new_archetype.hash set
  in
  Hashtbl.replace w.component_index (Component.id component) new_archetype_set

(* Remove a component from an entity *)
let remove_component w component_id entity =
  let archetype = get_archetype w entity in
  let archetype_edges = Archetype.edges archetype in
  let new_archetype =
    match Archetype.Edges.find_remove_opt archetype_edges component_id with
    | Some hash -> Hashtbl.find w.archetype_index hash
    | None ->
        let new_archetype =
          Archetype.create
            (Archetype.components archetype
            |> List.filter (fun c -> c <> component_id))
        in
        Archetype.Edges.replace_remove archetype_edges component_id
          (Some new_archetype.hash);
        new_archetype
  in
  Archetype.add_entity new_archetype entity
    (Archetype.extract_entity archetype entity
    |> List.filter (fun c -> Component.id c <> component_id));
  Hashtbl.replace w.archetype_index new_archetype.hash new_archetype;
  Hashtbl.replace w.entity_index entity new_archetype.hash;
  let new_archetype_set =
    match Hashtbl.find_opt w.component_index component_id with
    | None -> ArchetypeHashSet.singleton new_archetype.hash
    | Some set -> ArchetypeHashSet.add new_archetype.hash set
  in
  Hashtbl.replace w.component_index component_id new_archetype_set

(* Remove an entity from the world *)
let remove_entity w entity =
  let archetype = get_archetype w entity in
  Archetype.extract_entity archetype entity |> ignore;
  Hashtbl.remove w.entity_index entity
(* TODO: Should we remove the archetype if it's empty? i.e. should it be
   removed from the archetype index and/or its hash be removed from component index? *)

let add_system w schedule query system =
  System.Registry.register w.systems schedule query system

let evaluate_query w (query : Query.t) =
  (* 1. Fetch all "matching" archetypes
     2. For each archetype, fetch all entities and their components and only
        include the components that match the query *)
  let required_components =
    query.terms
    |> List.filter_map (function Query.Required c -> Some c | _ -> None)
  in
  let candidate_archetypes =
    required_components
    |> List.fold_left
         (fun acc c ->
           let set =
             Hashtbl.find_opt w.component_index c
             (* TODO: Could potentially short-circuit here if we find an empty set? *)
             |> Option.value ~default:ArchetypeHashSet.empty
           in
           match acc with
           | Some acc -> Some (ArchetypeHashSet.inter acc set)
           | None -> Some set)
         None
    |> Option.value ~default:ArchetypeHashSet.empty
  in
  candidate_archetypes |> ArchetypeHashSet.elements
  |> List.map (fun hash ->
         let archetype = Hashtbl.find w.archetype_index hash in
         Archetype.get_from_query archetype query)
  |> List.flatten

let run_systems w schedule =
  System.Registry.fetch w.systems schedule
  |> List.iter (fun (query, system) ->
         let result = evaluate_query w query in
         system result)
