module ArchetypeSet = Set.Make (Id.Archetype)

type t = {
  archetype_index : (Id.Archetype.t, Archetype.t) Hashtbl.t;
  (* TODO: entity id -> (archetype, row) *)
  entity_index : (Id.Entity.t, Archetype.t) Hashtbl.t;
  (* TODO: component id -> (archetype id -> column) *)
  component_index : (Id.Component.t, ArchetypeSet.t) Hashtbl.t;
}

let create =
  {
    archetype_index = Hashtbl.create 0;
    entity_index = Hashtbl.create 0;
    component_index = Hashtbl.create 0;
  }

let get_archetype t archetype =
  let archetype_id = Archetype.id archetype in
  match Hashtbl.find_opt t.archetype_index archetype_id with
  | Some archetype -> archetype
  | None ->
      Hashtbl.add t.archetype_index archetype_id archetype;
      archetype

let add_entity t =
  let entity = Id.Entity.next () in
  let empty_archetype = get_archetype t Archetype.empty in
  (* No need to modify component_index here since the entity has no components *)
  Hashtbl.add t.entity_index entity empty_archetype;
  entity

let get_component t component entity =
  let archetype = Hashtbl.find t.entity_index entity in
  ()
  (*let column =*)
  (*  Hashtbl.find_opt (Hashtbl.find t.component_index component) archetype.id*)
  (*in*)
  (*match column with*)
  (*| None -> None*)
  (*| Some column -> Some archetype.table.(column).(row)*)

let add_component t component entity =
  let current_archetype = Hashtbl.find t.entity_index entity in
  (* Do archetype graph search to find next archetype *)
  let next_archetype = current_archetype in
  (* Move entity to next archetype, move_entity *)
  Hashtbl.replace t.entity_index entity next_archetype

(*let add_entity t =*)
(*  let entity = Id.Entity.next () in*)
(*  Hashtbl.add t.entities entity (Hashtbl.create 0);*)
(*  entity*)
(**)
(*let remove_entity t entity = Hashtbl.remove t.entities entity*)
(**)
(*let add_component t entity component =*)
(*  let components = Hashtbl.find t.entities entity in*)
(*  let id = Component.id component in*)
(*  Hashtbl.replace components id component*)
(**)
(*let remove_component t entity component =*)
(*  let components = Hashtbl.find t.entities entity in*)
(*  let id = Component.id component in*)
(*  Hashtbl.remove components id*)
(**)
(*let add_system t system = t.systems <- system :: t.systems*)
(*let run_systems t = List.iter (fun system -> system t.entities) t.systems*)
(**)
(*let print t =*)
(*  Hashtbl.iter*)
(*    (fun entity components ->*)
(*      Printf.printf "Entity: %d\n" entity;*)
(*      Hashtbl.iter (fun id _ -> Printf.printf "- Component: %d\n" id) components)*)
(*    t.entities*)
