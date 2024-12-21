type schedule = Startup | Update
type system = Query.Result.t array -> unit

module Registry = struct
  type t = {
    mutable startup : (Query.t array * system) list;
    mutable update : (Query.t array * system) list;
  }

  let create () = { startup = []; update = [] }

  let register r schedule queries f =
    match schedule with
    | Startup -> r.startup <- (queries, f) :: r.startup
    | Update -> r.update <- (queries, f) :: r.update

  let fetch r schedule =
    (match schedule with Startup -> r.startup | Update -> r.update)
    |> List.rev
end
