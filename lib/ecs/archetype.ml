(* TODO: Archetype-based ECS *)

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

  let empty : ('a, 'b) t = Hashtbl.create 0

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
  table : (Id.Component.t, (Id.Entity.t, Component.value) Hashtbl.t) Hashtbl.t;
  (* edges[component] = (option<add>, option<remove>) *)
  edges : (Id.Component.t, Hash.t) Edges.t;
}

let empty =
  {
    hash = Hashtbl.hash [];
    components = Id.ComponentSet.empty;
    entities = Id.EntitySet.empty;
    table = Hashtbl.create 0;
    edges = Edges.empty;
  }

let create components =
  let components = Id.ComponentSet.of_list components in
  {
    hash = Hash.hash (components |> Id.ComponentSet.to_list);
    components;
    entities = Id.EntitySet.empty;
    table = Hashtbl.create 0;
    edges = Edges.empty;
  }

let hash a = a.hash
let components a = a.components |> Id.ComponentSet.to_list
let edges a = a.edges

(* Remove an entity from the archetype and return its components *)
let extract_entity a e : Component.value list =
  a.entities <- Id.EntitySet.remove e a.entities;
  Hashtbl.fold
    (fun _ table acc ->
      match Hashtbl.find_opt table e with
      | Some c -> c :: acc
      | None -> failwith "Entity not found, is this a bug?")
    a.table []

(* Add an entity to the archetype with the given components *)
let add_entity a e (components : Component.value list) =
  a.entities <- Id.EntitySet.add e a.entities;
  components
  |> List.iter (fun c ->
         let id = Component.id c in
         if not (Id.ComponentSet.mem id a.components) then
           failwith "Component not in archetype";
         match Hashtbl.find_opt a.table id with
         | Some table -> Hashtbl.add table e c
         | None ->
             let table = Hashtbl.create 0 in
             Hashtbl.add table e c;
             Hashtbl.add a.table id table)

let get_from_query a (query : Query.t) =
  if not (Query.Filter.matches query.filter a.components) then []
  else
    let exception Invalid_query in
    try
      (* entity -> component list *)
      let result = Hashtbl.create 0 in
      a.entities |> Id.EntitySet.iter (fun e -> Hashtbl.add result e []);
      query.terms
      |> List.rev
         (* Respond to the terms in reverse order since components are added in reverse *)
      |> List.iter (function
           | Query.Required c -> (
               match Hashtbl.find_opt a.table c with
               | Some table ->
                   (* This required component is present in the archetype, so we add it *)
                   Hashtbl.iter
                     (fun e c ->
                       let cs = Hashtbl.find_opt result e in
                       let cs = Option.value cs ~default:[] in
                       Hashtbl.replace result e (c :: cs))
                     table
               | None ->
                   (* This required component is not present in the archetype, so we short-circuit *)
                   raise Invalid_query)
           | Query.Optional c -> (
               match Hashtbl.find_opt a.table c with
               | Some table ->
                   (* This optional component is present in the archetype, so we add it *)
                   Hashtbl.iter
                     (fun e c ->
                       let cs = Hashtbl.find_opt result e in
                       let cs = Option.value cs ~default:[] in
                       Hashtbl.replace result e (c :: cs))
                     table
               | None ->
                   (* This optional component is not present in the archetype,
                      so we add a None component *)
                   a.entities
                   |> Id.EntitySet.iter (fun e ->
                          let cs = Hashtbl.find_opt result e in
                          let cs = Option.value cs ~default:[] in
                          Hashtbl.replace result e
                            (Component.make (module Component.None.C) () :: cs))
               )
           | Query.AnyOf _cs -> failwith "unimplemented");
      Hashtbl.fold (fun e cs acc -> (e, cs) :: acc) result []
    with Invalid_query -> []

let get_component a c e =
  match Hashtbl.find_opt a.table c with
  | Some table -> Hashtbl.find_opt table e
  | None -> None
