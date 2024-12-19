module ArchetypeHashSet = Set.Make (Archetype.Hash)

type t = {
  empty_archetype : Archetype.t;
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
    systems = System.Registry.create;
  }

let get_archetype w entity =
  Hashtbl.find w.archetype_index (Hashtbl.find w.entity_index entity)

(* Add a new entity and return its id *)
let add_entity w =
  let entity = Id.Entity.next () in
  Hashtbl.add w.entity_index entity (Archetype.hash w.empty_archetype);
  Archetype.add_entity w.empty_archetype entity [];
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
            (Archetype.components archetype
            |> Id.ComponentSet.add (Component.id component))
        in
        Archetype.Edges.replace_add archetype_edges (Component.id component)
          (Some (Archetype.hash new_archetype));
        new_archetype
  in
  (* Move entity from old archetype to new archetype *)
  Archetype.add_entity new_archetype entity
    (Archetype.extract_entity archetype entity @ [ component ]);
  Hashtbl.replace w.archetype_index (Archetype.hash new_archetype) new_archetype;
  Hashtbl.replace w.entity_index entity (Archetype.hash new_archetype);
  Archetype.components new_archetype
  |> Id.ComponentSet.to_seq
  |> Seq.iter (fun c ->
         let new_archetype_set =
           match Hashtbl.find_opt w.component_index c with
           | None -> ArchetypeHashSet.singleton (Archetype.hash new_archetype)
           | Some set -> ArchetypeHashSet.add (Archetype.hash new_archetype) set
         in
         Hashtbl.replace w.component_index c new_archetype_set)

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
            |> Id.ComponentSet.remove component_id)
        in
        Archetype.Edges.replace_remove archetype_edges component_id
          (Some (Archetype.hash new_archetype));
        new_archetype
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
  let required_components =
    Query.terms query
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
  candidate_archetypes |> ArchetypeHashSet.to_seq
  |> Seq.filter_map (fun hash ->
         let archetype = Hashtbl.find w.archetype_index hash in
         if
           Query.Filter.matches (Query.filter query)
             (Archetype.components archetype)
         then Some archetype
         else None)
  |> Seq.map (fun archetype ->
         Archetype.entities archetype
         |> Id.EntitySet.to_seq
         |> Seq.map (fun e ->
                ( e,
                  Query.terms query
                  |> List.map (function
                       | Query.Required c ->
                           Archetype.get_component archetype c e |> Option.get
                       | Query.Optional c ->
                           Archetype.get_component archetype c e
                           |> Option.value
                                ~default:
                                  (Component.make (module Component.None.C) ()))
                )))
  |> Seq.concat |> List.of_seq

let run_systems w schedule =
  System.Registry.fetch w.systems schedule
  |> List.iter (fun (query, system) ->
         let result = evaluate_query w query in
         system result)
