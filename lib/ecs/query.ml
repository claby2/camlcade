module Filter = struct
  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Wildcard

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
  | Req : (module Component.S with type t = 'a) -> 'a term
  | Opt : (module Component.S with type t = 'a) -> 'a option term

type _ t = Nil : unit t | Cons : 'a term * 'b t -> ('a * 'b) t

let rec required_ids : type a. a t -> _ = function
  | Nil -> Id.ComponentSet.empty
  | Cons (Req (module C), rest) -> Id.ComponentSet.add C.id (required_ids rest)
  | Cons (Opt (module C), rest) -> required_ids rest

let evaluate : type a.
    ?filter:Filter.t -> a t -> Archetype.t list -> (Id.Entity.t * a) list =
 fun ?(filter = Filter.Wildcard) query archetypes ->
  let rec fetch : type a. a t -> Archetype.t -> Id.Entity.t -> a =
   fun q a e ->
    let get_component = Archetype.query a e in
    match q with
    | Nil -> ()
    | Cons (Req (module C), rest) ->
        let c =
          get_component C.id |> Option.get |> Component.unpack (module C)
        in
        (c, fetch rest a e)
    | Cons (Opt (module C), rest) ->
        (* TODO: Rethink this maybe *)
        let c =
          get_component C.id
          |> Option.value ~default:(Component.pack (module Component.None.C) ())
          |> Component.unpack_opt (module C)
        in
        (c, fetch rest a e)
  in
  let required_ids = required_ids query in
  let is_candidate a =
    Id.ComponentSet.subset required_ids (Archetype.components a)
    && Filter.matches filter (Archetype.components a)
  in

  let build_result a =
    Archetype.entities a |> Id.EntitySet.to_list
    |> List.map (fun e -> (e, fetch query a e))
  in

  archetypes |> List.filter is_candidate |> List.concat_map build_result

let ( @ ) comp rest = Cons (comp, rest) (* Infix for QCons *)
