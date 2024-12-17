type t = {
  entities :
    (Id.Entity.t, (Id.Component.t, Component.value) Hashtbl.t) Hashtbl.t;
}

let create = { entities = Hashtbl.create 0 }

let add_entity t =
  let entity = Id.Entity.next () in
  Hashtbl.add t.entities entity (Hashtbl.create 0);
  entity

let remove_entity t entity = Hashtbl.remove t.entities entity

let add_component t entity component =
  let components = Hashtbl.find t.entities entity in
  let id = Component.id component in
  Hashtbl.replace components id component

let remove_component t entity component =
  let components = Hashtbl.find t.entities entity in
  let id = Component.id component in
  Hashtbl.remove components id

let print t =
  Hashtbl.iter
    (fun entity components ->
      Printf.printf "Entity: %d\n" entity;
      Hashtbl.iter (fun id _ -> Printf.printf "- Component: %d\n" id) components)
    t.entities
