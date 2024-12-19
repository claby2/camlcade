module Hash = struct
  type t = int

  let hash l = l |> List.sort compare |> Hashtbl.hash
  let compare = compare
end

module Edges = struct
  module Edge = struct
    type 'a t = { add : 'a option; remove : 'a option }

    let empty = { add = None; remove = None }
    let with_add e add = { e with add }
    let with_remove e remove = { e with remove }
  end

  type ('a, 'b) t = ('a, 'b Edge.t) Hashtbl.t

  let empty () : ('a, 'b) t = Hashtbl.create 0

  let find_add_opt (e : ('a, 'b) t) key =
    match Hashtbl.find_opt e key with Some edge -> edge.add | None -> None

  let find_remove_opt (e : ('a, 'b) t) key =
    match Hashtbl.find_opt e key with Some edge -> edge.remove | None -> None

  let replace_add e key value =
    match Hashtbl.find_opt e key with
    | Some edge -> Hashtbl.replace e key (Edge.with_add edge value)
    | None -> Hashtbl.add e key (Edge.with_add Edge.empty value)

  let replace_remove e key value =
    match Hashtbl.find_opt e key with
    | Some edge -> Hashtbl.replace e key (Edge.with_remove edge value)
    | None -> Hashtbl.add e key (Edge.with_remove Edge.empty value)
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
  let res =
    Id.ComponentSet.to_list a.components
    |> List.map (fun cid ->
           let table = Hashtbl.find a.table cid in
           match Hashtbl.find_opt table e with
           | Some c ->
               Hashtbl.remove table e;
               c
           | None -> failwith "Entity not found, is this a bug?")
  in
  a.entities <- Id.EntitySet.remove e a.entities;
  res

exception Invalid_components

(* Add an entity to the archetype with the given components *)
let add_entity a e components =
  (* Convert components to a hash table from component id to component value *)
  let components =
    Hashtbl.of_seq
      (components |> List.to_seq |> Seq.map (fun c -> (Component.id c, c)))
  in
  if Hashtbl.length components <> Id.ComponentSet.cardinal a.components then
    raise Invalid_components;
  components |> Hashtbl.to_seq
  |> Seq.iter (fun (id, c) ->
         if not (Id.ComponentSet.mem id a.components) then
           raise Invalid_components;
         match Hashtbl.find_opt a.table id with
         | Some table -> Hashtbl.add table e c
         | None ->
             let table = Hashtbl.create 0 in
             Hashtbl.add table e c;
             Hashtbl.add a.table id table);
  a.entities <- Id.EntitySet.add e a.entities

let get_component a c e =
  match Hashtbl.find_opt a.table c with
  | Some table -> Hashtbl.find_opt table e
  | None -> None
