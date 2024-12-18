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

(* Add a new entity and return its id *)
let add_entity t =
  let entity = Id.Entity.next () in
  Hashtbl.add t.entity_index entity (Archetype.hash Archetype.empty);
  entity

(* Get the value of a specific component for a given entity if it exists *)
let get_component t component entity =
  match Hashtbl.find_opt t.entity_index entity with
  | Some archetype_id -> (
      match Hashtbl.find_opt t.archetype_index archetype_id with
      | Some archetype -> Archetype.get_component archetype component entity
      | None -> None)
  | None -> None

(* Add a component to an entity *)
let add_component t (component : Component.value) entity =
  let archetype_id = Hashtbl.find t.entity_index entity in
  let archetype = Hashtbl.find t.archetype_index archetype_id in
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
    (component :: Archetype.extract_entity archetype entity);
  Hashtbl.replace t.archetype_index new_archetype.hash new_archetype;
  Hashtbl.replace t.entity_index entity new_archetype.hash

(* Remove a component from an entity *)
let remove_component _t _component _entity = failwith "Not implemented"

(* Remove an entity from the world *)
let remove_entity _t _entity = failwith "Not implemented"
