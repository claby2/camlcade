(* TODO: Archetype-based ECS *)

module Edge = struct
  type 'a t = { add : 'a option; remove : 'a option }

  let empty = { add = None; remove = None }
  let with_add e add = { e with add = Some add }
  let with_remove e remove = { e with remove = Some remove }
  let remove_add e = { e with add = None }
  let remove_remove e = { e with remove = None }
end

type t = {
  id : Id.Archetype.t;
  components : Id.Component.t list;
  (*table : Component.value array array;*)
  (* TODO: Use SparseSet *)
  table : (Id.Component.t, (Id.Entity.t, Component.value) Hashtbl.t) Hashtbl.t;
  edges : (Id.Component.t, t Edge.t) Hashtbl.t;
}

let empty =
  {
    id = Id.Archetype.next ();
    components = [];
    table = Hashtbl.create 0;
    edges = Hashtbl.create 0;
  }

let id t = t.id

let add_component t entity component =
  match Hashtbl.find_opt t.edges (Component.id component) with
  | Some { add = Some t'; _ } ->
      t.table
      |> Hashtbl.iter (fun component' entities ->
             match Hashtbl.find_opt entities entity with
             | Some value ->
                 Hashtbl.add (Hashtbl.find t'.table component') entity value;
                 Hashtbl.remove entities entity
             | None ->
                 failwith "Entity not found in table, maybe this is a bug?");
      Some t'
  | Some { add = None; _ } ->
      let t' =
        {
          id = Id.Archetype.next ();
          components = Component.id component :: t.components;
          table = Hashtbl.create 0;
          edges = Hashtbl.create 0;
        }
      in
      Hashtbl.add t.edges (Component.id component) (Edge.with_add Edge.empty t');
      t.table
      |> Hashtbl.iter (fun component' entities ->
             match Hashtbl.find_opt entities entity with
             | Some value ->
                 Hashtbl.add (Hashtbl.find t'.table component') entity value;
                 Hashtbl.remove entities entity
             | None ->
                 failwith "Entity not found in table, maybe this is a bug?");
      Some t'
  | None -> None
