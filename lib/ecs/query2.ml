module Filter = struct
  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Wildcard

  (* Returns true if the given component set matches the filter *)
  let matches f components =
    let rec aux f components =
      match f with
      | With c -> Id.ComponentSet.mem c components
      | Without c -> not (Id.ComponentSet.mem c components)
      | Not f -> not (aux f components)
      | And (f1, f2) -> aux f1 components && aux f2 components
      | Or (f1, f2) -> aux f1 components || aux f2 components
      | Wildcard -> true
    in
    aux f components
end

type _ term =
  | Required : (module Component.S with type t = 'a) -> 'a term
  | Optional : (module Component.S with type t = 'a) -> 'a option term

type _ t = QNil : unit t | QCons : 'a term * 'b t -> ('a * 'b) t

let rec required : type a. a t -> _ = function
  | QNil -> Id.ComponentSet.empty
  | QCons (Required (module C), rest) ->
      Id.ComponentSet.add C.id (required rest)
  | QCons (Optional (module C), rest) -> required rest

let evaluate : type a.
    Filter.t -> a t -> Archetype.t list -> (Id.Entity.t * a) list =
 fun filter query archetypes ->
  let rec fetch : type a. a t -> Archetype.t -> Id.Entity.t -> a =
   fun q a e ->
    let get_component = Archetype.query a e in
    match q with
    | QNil -> ()
    | QCons (Required (module C), rest) ->
        let c =
          get_component C.id |> Option.get |> Component.unpack (module C)
        in
        (c, fetch rest a e)
    | QCons (Optional (module C), rest) ->
        (* TODO: Rethink this maybe *)
        let c =
          get_component C.id
          |> Option.value ~default:(Component.pack (module Component.None.C) ())
          |> Component.unpack_opt (module C)
        in
        (c, fetch rest a e)
  in
  let matches_filter a = Filter.matches filter (Archetype.components a) in

  let build_result a =
    Archetype.entities a |> Id.EntitySet.to_list
    |> List.map (fun e -> (e, fetch query a e))
  in

  archetypes |> List.filter matches_filter |> List.concat_map build_result

type _ set =
  | Single : 'a. 'a t -> (Id.Entity.t * 'a) list set
  | Cons : 'a t * 'b set -> ('a t * 'b) set

module Foo = struct
  type t = float

  module C = Component.Make (struct
    type inner = t
  end)
end

module Bar = struct
  type t = int

  module C = Component.Make (struct
    type inner = t
  end)
end

let ( ^^ ) comp rest = QCons (comp, rest) (* Infix for QCons *)

let test_query =
  QCons (Required (module Foo.C), QCons (Required (module Bar.C), QNil))

let test_query2 = Required (module Foo.C) ^^ Required (module Bar.C) ^^ QNil

let test_query_set =
  Single (Required (module Foo.C) ^^ Required (module Bar.C) ^^ QNil)

let test_query_set2 =
  Cons (Required (module Foo.C) ^^ Required (module Bar.C) ^^ QNil, Single QNil)

let test archetypes =
  let result = evaluate Filter.Wildcard test_query archetypes in
  List.iter (fun (_entity, (_foo, (_bar, ()))) -> ()) result;
  ()
