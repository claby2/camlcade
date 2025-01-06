(* A system should be able to have multiple queries

   A system should be parallelizable
   - aka Know which archetypes it should consider ahead of time


let simulate_ball =
    System.create ()
    |> with_query Query.(Required (module Input.Keyboard.C) ^^ QNil)
    |> with_query Query.(Required (module Transform.C) ^^ Optional (module Ball.C) ^^ QNil)
    |> with_query ~filter:Filter.(With Ball.C.id) Query.(Required Transform.C.id ^^ QNil)



    |> with_query (QCons (Required (module Transform.C), QCons (Optional (module Transform.C), QNil)))
    |> with_query (QCons (Required (module Input.Keyboard.C), QNil))


How can I compose multiple queries to the same system?

Introduce a QuerySet:

type _ query_set = Single : unit query_set | Multiple : 'a Query.t * 'b query_set -> ('a * 'b) query_set

let simulate_ball =
    System.create
        ~query:(Single (Query.(Required (module Input.Keyboard.C) ^^ QNil)))
        ~operation:(fun (keyboards) -> ...)

let simulate_ball2 =
    System.create
        ~operation:(fun (keyboards) -> ...)


   *)

type _ t =
  | Nil : unit t
  | Cons : {
      filter : Query2.Filter.t;
      query : 'a Query2.t;
      rest : 'b t;
    }
      -> ((Id.Entity.t * 'a) list * 'b) t

(* TODO: Cache this *)
let rec required : type a. a t -> Id.ComponentSet.t = function
  | Nil -> Id.ComponentSet.empty
  | Cons { query; rest; _ } ->
      Id.ComponentSet.union (Query2.required query) (required rest)

let create () = Nil

let with_query ?(filter = Query2.Filter.Wildcard) query rest =
  Cons { filter; query; rest }

let rec evaluate : type r. r t -> Archetype.t list -> r =
 fun sys archetypes ->
  match sys with
  | Nil -> ()
  | Cons { filter; query; rest } ->
      let result = Query2.evaluate filter query archetypes in
      (result, evaluate rest archetypes)

(*
type any_query =
  | AnyQuery : { filter : Query2.Filter.t; query : 'a Query2.t } -> any_query

type t = { queries : any_query list; required : Id.ComponentSet.t }

let create () = { queries = []; required = Id.ComponentSet.empty }

let with_query ?(filter = Query2.Filter.Wildcard) query t =
  {
    queries = AnyQuery { filter; query } :: t.queries;
    required = Id.ComponentSet.union t.required (Query2.required query);
  }

let required { required; _ } = required

type any_result = AnyResult : 'a -> any_result

let evaluate t archetypes =
  let rec aux acc = function
    | [] -> List.rev acc
    | AnyQuery { filter; query } :: rest ->
        let results = Query2.evaluate filter query archetypes in
        aux (AnyResult results :: acc) rest
  in
  aux [] t.queries
*)
