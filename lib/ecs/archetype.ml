module Hash = struct
  type t = int

  let hash l = l |> List.sort compare |> Hashtbl.hash
  let compare = compare
end

module Edges = struct
  module Edge = struct
    type 'a t = { add : 'a option; remove : 'a option }

    let empty = { add = None; remove = None }
    let set_add e add = { e with add }
    let set_remove e remove = { e with remove }
    let add e = e.add
    let remove e = e.remove
  end

  type ('a, 'b) t = ('a, 'b Edge.t) Hashtbl.t

  let empty () = Hashtbl.create 0

  let find_add_opt e key =
    match Hashtbl.find_opt e key with
    | Some edge -> Edge.add edge
    | None -> None

  let find_remove_opt e key =
    match Hashtbl.find_opt e key with
    | Some edge -> Edge.remove edge
    | None -> None

  let replace_add e key value =
    match Hashtbl.find_opt e key with
    | Some edge -> Hashtbl.replace e key (Edge.set_add edge value)
    | None -> Hashtbl.add e key (Edge.set_add Edge.empty value)

  let replace_remove e key value =
    match Hashtbl.find_opt e key with
    | Some edge -> Hashtbl.replace e key (Edge.set_remove edge value)
    | None -> Hashtbl.add e key (Edge.set_remove Edge.empty value)
end

type t = {
  hash : Hash.t;
  components : Id.ComponentSet.t;
  mutable entities : Id.EntitySet.t;
  (* TODO: Use SparseSet *)
  table : (Id.Component.t, (Id.Entity.t, Component.packed) Hashtbl.t) Hashtbl.t;
  (* edges[component] = (option<add>, option<remove>) *)
  edges : (Id.Component.t, Hash.t) Edges.t;
}

let to_string a =
  Printf.sprintf "{ hash = %d; entities = %s; table = %s; }" a.hash
    (a.entities |> Id.EntitySet.to_seq
    |> Seq.fold_left (fun acc e -> Printf.sprintf "%s%d, " acc e) "")
    (Hashtbl.fold
       (fun k v acc ->
         Printf.sprintf "%s%d -> %s\n" acc k
           (Hashtbl.fold
              (fun k _v acc -> Printf.sprintf "%s%d -> _\n" acc k)
              v ""))
       a.table "")

let empty () =
  {
    hash = Hashtbl.hash [];
    components = Id.ComponentSet.empty;
    entities = Id.EntitySet.empty;
    table = Hashtbl.create 0;
    edges = Edges.empty ();
  }

let create components =
  {
    hash = Hash.hash (components |> Id.ComponentSet.to_list);
    components;
    entities = Id.EntitySet.empty;
    table = Hashtbl.create 0;
    edges = Edges.empty ();
  }

let hash a = a.hash
let components a = a.components
let edges a = a.edges
let entities a = a.entities

exception Entity_not_found

(* Remove an entity from the archetype and return its components *)
let extract_entity a e =
  if not (Id.EntitySet.mem e a.entities) then raise Entity_not_found;

  (* Gather all components for the entity before mutating the archetype *)
  let components = Id.ComponentSet.to_list a.components in
  let extracted =
    components
    |> List.map (fun cid ->
           let table = Hashtbl.find a.table cid in
           let component = Hashtbl.find table e in
           (cid, component))
  in

  (* Now we know all components exist, we can safely remove the entity *)
  extracted
  |> List.iter (fun (cid, _) ->
         let table = Hashtbl.find a.table cid in
         Hashtbl.remove table e);

  (* Finally, remove the entity from the entity set *)
  a.entities <- Id.EntitySet.remove e a.entities;

  (* Return the packed components *)
  extracted |> List.map snd

exception Invalid_components

(* Add an entity to the archetype with the given components *)
let add_entity a e components =
  (* Convert components to a hash table from component id to component value *)
  let components =
    Hashtbl.of_seq
      (List.to_seq components |> Seq.map (fun c -> (Component.id c, c)))
  in

  (* Check that the given components match the archetype's components *)
  let expected_count = Id.ComponentSet.cardinal a.components in
  let actual_count = Hashtbl.length components in
  if expected_count <> actual_count then raise Invalid_components;
  Id.ComponentSet.iter
    (fun cid ->
      if not (Hashtbl.mem components cid) then raise Invalid_components)
    a.components;

  (* Add the components to the archetype *)
  Hashtbl.iter
    (fun cid c ->
      match Hashtbl.find_opt a.table cid with
      | Some table -> Hashtbl.add table e c
      | None ->
          let table = Hashtbl.create 0 in
          Hashtbl.add table e c;
          Hashtbl.add a.table cid table)
    components;

  (* Add the entity to the entity set *)
  a.entities <- Id.EntitySet.add e a.entities

let get_component a c e =
  match Hashtbl.find_opt a.table c with
  | Some table -> Hashtbl.find_opt table e
  | None -> None
