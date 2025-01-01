open Storage

let calculate_hash l =
  (* Convert to a string first rather than operate on the list directly
       since Hashtbl.hash stops iterating through a list after 10 elements *)
  l |> List.sort compare
  |> List.map (fun v -> v |> Id.Component.to_int |> string_of_int)
  |> String.concat "" |> Hashtbl.hash

type operation = Add of Id.Component.t | Remove of Id.Component.t

type t = {
  components : Id.ComponentSet.t;
  hash : int;
  (* table[component.id][entity.id] = entity's component *)
  table : Component.packed Sparse_set.t Sparse_set.t;
  add_hashes : (Id.Component.t, int) Hashtbl.t;
  remove_hashes : (Id.Component.t, int) Hashtbl.t;
  mutable entities : Id.EntitySet.t;
}

let create components =
  let table = Sparse_set.create () in
  Id.ComponentSet.iter
    (fun cid ->
      Sparse_set.set table (Id.Component.to_int cid) (Sparse_set.create ()))
    components;
  {
    components;
    hash = calculate_hash (Id.ComponentSet.to_list components);
    table;
    add_hashes = Hashtbl.create 0;
    remove_hashes = Hashtbl.create 0;
    entities = Id.EntitySet.empty;
  }

let empty () = create Id.ComponentSet.empty
let hash a = a.hash
let components a = a.components
let entities a = a.entities

let query a eid cid =
  let eid = Id.Entity.to_int eid in
  let cid = Id.Component.to_int cid in
  Option.bind (Sparse_set.get a.table cid) (fun c -> Sparse_set.get c eid)

let remove a eid =
  let remove () = a.entities <- Id.EntitySet.remove eid a.entities in
  let eid = Id.Entity.to_int eid in
  a.components
  |> Id.ComponentSet.iter (fun cid ->
         let cid = Id.Component.to_int cid in
         match Sparse_set.get a.table cid with
         | None -> failwith "invariant violated, table missing component"
         | Some c -> Sparse_set.remove c eid |> ignore);
  remove ()

let add a eid components =
  let add () = a.entities <- Id.EntitySet.add eid a.entities in
  let eid = Id.Entity.to_int eid in
  let validate_components l =
    Id.ComponentSet.equal a.components
      (l |> List.map Component.id |> Id.ComponentSet.of_list)
  in
  if not (validate_components components) then
    invalid_arg "tried to add foreign component";

  components
  |> List.iter (fun comp ->
         let cid = Id.Component.to_int (Component.id comp) in
         match Sparse_set.get a.table cid with
         | None -> failwith "invariant violated, table missing component"
         | Some c -> Sparse_set.set c eid comp);
  add ()

let replace a eid component =
  let eid = Id.Entity.to_int eid in
  let cid = Id.Component.to_int (Component.id component) in
  match Sparse_set.get a.table cid with
  | None -> invalid_arg "component not found"
  | Some c -> Sparse_set.set c eid component

let next_hash a op =
  let find_or_compute tbl key compute =
    match Hashtbl.find_opt tbl key with
    | Some h -> h
    | None ->
        let h = compute () in
        Hashtbl.add tbl key h;
        h
  in
  match op with
  | Add cid ->
      if Id.ComponentSet.mem cid a.components then a.hash
      else
        find_or_compute a.add_hashes cid (fun () ->
            calculate_hash
              (Id.ComponentSet.to_list (Id.ComponentSet.add cid a.components)))
  | Remove cid ->
      if not (Id.ComponentSet.mem cid a.components) then a.hash
      else
        find_or_compute a.remove_hashes cid (fun () ->
            calculate_hash
              (Id.ComponentSet.to_list
                 (Id.ComponentSet.remove cid a.components)))
