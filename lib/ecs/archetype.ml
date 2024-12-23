module Hash = struct
  type t = int

  let hash l =
    (* Convert to a string first rather than operate on the list directly
       since Hashtbl.hash stops iterating through a list after 10 elements *)
    l |> List.sort compare
    |> List.map (fun v -> v |> Id.Component.to_int |> string_of_int)
    |> String.concat "" |> Hashtbl.hash

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
  table : (Id.Entity.t, Component.packed) Hashtbl.t array;
  (* edges[component] = (option<add>, option<remove>) *)
  edges : (Id.Component.t, Hash.t) Edges.t;
}

let empty () =
  {
    hash = Hashtbl.hash [];
    components = Id.ComponentSet.empty;
    entities = Id.EntitySet.empty;
    table = Array.make 0 (Hashtbl.create 0);
    edges = Edges.empty ();
  }

let create components =
  let max_id = Id.ComponentSet.max_elt components |> Id.Component.to_int in
  {
    hash = Hash.hash (components |> Id.ComponentSet.to_list);
    components;
    entities = Id.EntitySet.empty;
    table = Array.init (max_id + 1) (fun _ -> Hashtbl.create 0);
    edges = Edges.empty ();
  }

let hash a = a.hash

let hash_with_component a c =
  if Id.ComponentSet.mem c a.components then a.hash
  else
    match Edges.find_add_opt a.edges c with
    | Some hash -> hash
    | None ->
        let new_components =
          Id.ComponentSet.add c a.components |> Id.ComponentSet.to_list
        in
        let hash = Hash.hash new_components in
        Edges.replace_add a.edges c (Some hash);
        hash

let hash_without_component a c =
  if not (Id.ComponentSet.mem c a.components) then a.hash
  else
    match Edges.find_remove_opt a.edges c with
    | Some hash -> hash
    | None ->
        let new_components =
          Id.ComponentSet.remove c a.components |> Id.ComponentSet.to_list
        in
        let hash = Hash.hash new_components in
        Edges.replace_remove a.edges c (Some hash);
        hash

let components a = a.components
let entities a = a.entities

let get_component a e c =
  let c = Id.Component.to_int c in
  if c >= Array.length a.table then None
  else
    let table = Array.get a.table c in
    Hashtbl.find_opt table e

let get_entity a e =
  if not (Id.EntitySet.mem e a.entities) then raise Not_found;

  let components = Id.ComponentSet.to_list a.components in
  components |> List.map (fun cid -> (cid, get_component a e cid |> Option.get))

(* Remove an entity from the archetype and return its components *)
let extract_entity a e =
  let extracted = get_entity a e in
  (* Now we know all components exist, we can safely remove the entity *)
  extracted
  |> List.iter (fun (cid, _) ->
         let cid = Id.Component.to_int cid in
         let table = Array.get a.table cid in
         Hashtbl.remove table e);

  (* Finally, remove the entity from the entity set *)
  a.entities <- Id.EntitySet.remove e a.entities;

  (* Return the packed components *)
  extracted |> List.map snd

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
  if expected_count <> actual_count then
    invalid_arg "number of components do not match";
  Id.ComponentSet.iter
    (fun cid ->
      if not (Hashtbl.mem components cid) then
        invalid_arg "found invalid component")
    a.components;

  (* Add the components to the archetype *)
  Hashtbl.iter
    (fun cid c ->
      let cid = Id.Component.to_int cid in
      let table = Array.get a.table cid in
      Hashtbl.replace table e c)
    components;

  (* Add the entity to the entity set *)
  a.entities <- Id.EntitySet.add e a.entities
