type schedule = Startup | Update
type system = Query.Result.t array -> unit

module Registry = struct
  type t = (schedule, (Query.t array * system) list) Hashtbl.t

  let create () = Hashtbl.create 0

  let register r schedule queries f =
    let current = Hashtbl.find_opt r schedule in
    let current = Option.value current ~default:[] in
    Hashtbl.replace r schedule ((queries, f) :: current)

  let fetch r schedule =
    let systems = Hashtbl.find_opt r schedule in
    Option.value systems ~default:[] |> List.rev
end
