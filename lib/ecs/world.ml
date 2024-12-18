module ArchetypeHashSet = Set.Make (Archetype.Hash)

type t = {
  archetype_index : (Archetype.Hash.t, Archetype.t) Hashtbl.t;
  (* TODO: entity id -> (archetype, row) *)
  entity_index : (Id.Entity.t, Archetype.Hash.t) Hashtbl.t;
  (* TODO: component id -> (archetype id -> column) *)
  component_index : (Id.Component.t, ArchetypeHashSet.t) Hashtbl.t;
}

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
  }

let get_archetype t entity =
  Hashtbl.find t.archetype_index (Hashtbl.find t.entity_index entity)

(* Add a new entity and return its id *)
let add_entity t =
  let entity = Id.Entity.next () in
  Hashtbl.add t.entity_index entity (Archetype.hash Archetype.empty);
  entity

(* Get the value of a specific component for a given entity if it exists *)
let get_component t component entity =
  let archetype = get_archetype t entity in
  Archetype.get_component archetype component entity

(* Add a component to an entity *)
let add_component t component entity =
  let archetype = get_archetype t entity in
  let archetype_edges = Archetype.edges archetype in
  let new_archetype =
    match
      Archetype.Edges.find_add_opt archetype_edges (Component.id component)
    with
    | Some hash -> Hashtbl.find t.archetype_index hash
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
  Hashtbl.replace t.archetype_index new_archetype.hash new_archetype;
  Hashtbl.replace t.entity_index entity new_archetype.hash;
  let new_archetype_set =
    match Hashtbl.find_opt t.component_index (Component.id component) with
    | None -> ArchetypeHashSet.singleton new_archetype.hash
    | Some set -> ArchetypeHashSet.add new_archetype.hash set
  in
  Hashtbl.replace t.component_index (Component.id component) new_archetype_set

(* Remove a component from an entity *)
let remove_component t component_id entity =
  let archetype = get_archetype t entity in
  let archetype_edges = Archetype.edges archetype in
  let new_archetype =
    match Archetype.Edges.find_remove_opt archetype_edges component_id with
    | Some hash -> Hashtbl.find t.archetype_index hash
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
  Hashtbl.replace t.archetype_index new_archetype.hash new_archetype;
  Hashtbl.replace t.entity_index entity new_archetype.hash;
  let new_archetype_set =
    match Hashtbl.find_opt t.component_index component_id with
    | None -> ArchetypeHashSet.singleton new_archetype.hash
    | Some set -> ArchetypeHashSet.add new_archetype.hash set
  in
  Hashtbl.replace t.component_index component_id new_archetype_set

(* Remove an entity from the world *)
let remove_entity t entity =
  let archetype = get_archetype t entity in
  Archetype.extract_entity archetype entity |> ignore;
  Hashtbl.remove t.entity_index entity
(* TODO: Should we remove the archetype if it's empty? i.e. should it be
   removed from the archetype index and/or its hash be removed from component index? *)
