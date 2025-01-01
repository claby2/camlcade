module ArchetypeHashSet = Set.Make (Int)

type t = {
  empty_archetype : Archetype.t;
  archetype_index : (int, Archetype.t) Hashtbl.t;
  entity_index : (Id.Entity.t, int) Hashtbl.t;
  component_index : (Id.Component.t, ArchetypeHashSet.t) Hashtbl.t;
  scheduler : t System.t Scheduler.t;
  mutable quit : bool;
}

let create () =
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

let add_entity w =
  let entity = Id.Entity.next () in
  Hashtbl.add w.entity_index entity (Archetype.hash w.empty_archetype);
  Archetype.add w.empty_archetype entity [];
  entity

let remove_entity w entity =
  let arch = get_archetype w entity in
  Archetype.remove arch entity;
  Hashtbl.remove w.entity_index entity
(* TODO: Should we remove the archetype if it's empty? i.e. should it be
   removed from the archetype index and/or its hash be removed from component index? *)

let entities w = w.entity_index |> Hashtbl.to_seq_keys |> List.of_seq

let get_new_archetype w old_arch operation =
  let next_hash = Archetype.next_hash old_arch operation in
  match Hashtbl.find_opt w.archetype_index next_hash with
  | Some a -> a
  | None ->
      let cid, operation =
        match operation with
        | Archetype.Add cid -> (cid, Id.ComponentSet.add)
        | Archetype.Remove cid -> (cid, Id.ComponentSet.remove)
      in
      let a =
        Archetype.create (operation cid (Archetype.components old_arch))
      in
      Hashtbl.add w.archetype_index next_hash a;
      a

let extract_from_archetype arch entity components =
  let l =
    components |> Id.ComponentSet.to_list
    |> List.map (fun cid -> Archetype.query arch entity cid |> Option.get)
  in
  Archetype.remove arch entity;
  l

let update_component_index w arch =
  let operation =
    if Id.EntitySet.is_empty (Archetype.entities arch) then
      ArchetypeHashSet.remove
    else ArchetypeHashSet.add
  in
  Archetype.components arch
  |> Id.ComponentSet.iter (fun cid ->
         let arch_set =
           Option.value
             (Hashtbl.find_opt w.component_index cid)
             ~default:ArchetypeHashSet.empty
         in
         Hashtbl.replace w.component_index cid
           (operation (Archetype.hash arch) arch_set))

let add_component w component entity =
  let old_arch = get_archetype w entity in
  let new_arch =
    get_new_archetype w old_arch (Archetype.Add (Component.id component))
  in
  if Archetype.hash old_arch = Archetype.hash new_arch then
    Archetype.replace old_arch entity component
  else (
    Hashtbl.replace w.entity_index entity (Archetype.hash new_arch);
    Archetype.add new_arch entity
      (component
      :: extract_from_archetype old_arch entity (Archetype.components old_arch)
      );
    update_component_index w old_arch;
    update_component_index w new_arch)

let with_component : type a.
    t -> (module Component.S with type t = a) -> a -> Id.Entity.t -> Id.Entity.t
    =
 fun w (module C) component entity ->
  add_component w (Component.pack (module C) component) entity;
  entity

let remove_component w component_id entity =
  let old_arch = get_archetype w entity in
  let new_arch = get_new_archetype w old_arch (Archetype.Remove component_id) in
  if Archetype.hash old_arch = Archetype.hash new_arch then ()
  else
    let components =
      extract_from_archetype old_arch entity (Archetype.components new_arch)
    in
    Archetype.add new_arch entity components;
    Hashtbl.replace w.entity_index entity (Archetype.hash new_arch);
    update_component_index w old_arch;
    update_component_index w new_arch

let get_component w entity component =
  try
    let archetype = get_archetype w entity in
    Archetype.query archetype entity component
  with Not_found -> None

let add_system w schedule system =
  Scheduler.register w.scheduler schedule system

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
  let run_system system =
    try System.run w (evaluate_query w) system with Quit -> w.quit <- true
  in
  Scheduler.fetch w.scheduler schedule |> List.iter run_system

let has_quit w = w.quit
