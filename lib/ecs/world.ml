module ArchetypeHashSet = Set.Make (Archetype.Hash)

type t = {
  empty_archetype : Archetype.t;
  archetype_index : (Archetype.Hash.t, Archetype.t) Hashtbl.t;
  (* TODO: entity id -> (archetype, row) *)
  entity_index : (Id.Entity.t, Archetype.Hash.t) Hashtbl.t;
  (* TODO: component id -> (archetype id -> column) *)
  component_index : (Id.Component.t, ArchetypeHashSet.t) Hashtbl.t;
  scheduler : (Query.t array * t System.t) Scheduler.t;
  mutable quit : bool;
}

(* Create a new empty world *)
let create () =
  (* Start with the empty archetype in the archetype index *)
  let archetype_index = Hashtbl.create 0 in
  let empty_archetype = Archetype.empty () in
  Hashtbl.add archetype_index (Archetype.hash empty_archetype) empty_archetype;
  {
    empty_archetype;
    archetype_index;
    entity_index = Hashtbl.create 0;
    component_index = Hashtbl.create 0;
    scheduler = Scheduler.create ();
    quit = false;
  }

let get_archetype w entity =
  Hashtbl.find w.archetype_index (Hashtbl.find w.entity_index entity)

(* Add a new entity and return its id *)
let add_entity w =
  let entity = Id.Entity.next () in
  Hashtbl.add w.entity_index entity (Archetype.hash w.empty_archetype);
  Archetype.add_entity w.empty_archetype entity [];
  entity

(* Remove an entity from the world *)
let remove_entity w entity =
  let archetype = get_archetype w entity in
  Archetype.extract_entity archetype entity |> ignore;
  Hashtbl.remove w.entity_index entity
(* TODO: Should we remove the archetype if it's empty? i.e. should it be
   removed from the archetype index and/or its hash be removed from component index? *)

let entities w = w.entity_index |> Hashtbl.to_seq_keys |> List.of_seq

let create_archetype_with_components w components =
  let new_archetype = Archetype.create components in
  if Hashtbl.mem w.archetype_index (Archetype.hash new_archetype) then
    Hashtbl.find w.archetype_index (Archetype.hash new_archetype)
  else (
    Hashtbl.add w.archetype_index (Archetype.hash new_archetype) new_archetype;
    new_archetype)

(* Add a component to an entity *)
let add_component w component entity =
  let archetype = get_archetype w entity in
  let new_archetype_hash =
    Archetype.hash_with_component archetype (Component.id component)
  in
  let new_archetype =
    match Hashtbl.find_opt w.archetype_index new_archetype_hash with
    | Some a -> a
    | None ->
        create_archetype_with_components w
          (Archetype.components archetype
          |> Id.ComponentSet.add (Component.id component))
  in
  (* Move entity from old archetype to new archetype *)
  Archetype.add_entity new_archetype entity
    (Archetype.extract_entity archetype entity @ [ component ]);
  Hashtbl.replace w.archetype_index (Archetype.hash new_archetype) new_archetype;
  Hashtbl.replace w.entity_index entity (Archetype.hash new_archetype);
  Archetype.components new_archetype
  |> Id.ComponentSet.iter (fun c ->
         let new_archetype_set =
           match Hashtbl.find_opt w.component_index c with
           | None -> ArchetypeHashSet.singleton (Archetype.hash new_archetype)
           | Some set -> ArchetypeHashSet.add (Archetype.hash new_archetype) set
         in
         Hashtbl.replace w.component_index c new_archetype_set)

let with_component : type a.
    t -> (module Component.S with type t = a) -> a -> Id.Entity.t -> Id.Entity.t
    =
 fun w (module C) component entity ->
  add_component w (Component.pack (module C) component) entity;
  entity

(* Remove a component from an entity *)
let remove_component w component_id entity =
  let archetype = get_archetype w entity in
  let new_archetype_hash =
    Archetype.hash_without_component archetype component_id
  in
  let new_archetype =
    match Hashtbl.find_opt w.archetype_index new_archetype_hash with
    | Some a -> a
    | None ->
        create_archetype_with_components w
          (Archetype.components archetype |> Id.ComponentSet.remove component_id)
  in
  Archetype.add_entity new_archetype entity
    (Archetype.extract_entity archetype entity
    |> List.filter (fun c -> Component.id c <> component_id));
  Hashtbl.replace w.archetype_index (Archetype.hash new_archetype) new_archetype;
  Hashtbl.replace w.entity_index entity (Archetype.hash new_archetype);
  let new_archetype_set =
    match Hashtbl.find_opt w.component_index component_id with
    | None -> ArchetypeHashSet.singleton (Archetype.hash new_archetype)
    | Some set -> ArchetypeHashSet.add (Archetype.hash new_archetype) set
  in
  Hashtbl.replace w.component_index component_id new_archetype_set

(* Get the value of a specific component for a given entity if it exists *)
let get_component w entity component =
  try
    let archetype = get_archetype w entity in
    Archetype.get_component archetype entity component
  with Not_found -> None

let add_system w schedule queries system =
  Scheduler.register w.scheduler schedule (queries, system)

let evaluate_query w (query : Query.t) =
  let required_components = Query.required_components query in
  let intersection_opt acc c =
    let set =
      match Hashtbl.find_opt w.component_index c with
      | Some set -> set
      | None -> ArchetypeHashSet.empty
    in
    match acc with
    | Some acc -> Some (ArchetypeHashSet.inter acc set)
    | None -> Some set
  in
  let candidate_archetypes =
    if List.is_empty required_components then
      (* There are no required components, so the candidate archetypes is the set of all archetypes *)
      Hashtbl.to_seq_values w.archetype_index |> List.of_seq
    else
      (* There are required components, so the candidate archetypes is the intersection of the sets
         of archetypes that contain each required component *)
      required_components
      |> List.fold_left intersection_opt None
      |> Option.value ~default:ArchetypeHashSet.empty
      |> ArchetypeHashSet.to_list
      |> List.map (Hashtbl.find w.archetype_index)
  in
  Query.evaluate query candidate_archetypes

exception Quit

let run_systems w schedule =
  let run_system (queries, system) =
    let result = queries |> Array.map (evaluate_query w) in
    try
      match system with
      | System.Query system -> system result
      | System.Immediate system -> system w result
    with Quit -> w.quit <- true
  in
  Scheduler.fetch w.scheduler schedule |> List.iter run_system

let has_quit w = w.quit
