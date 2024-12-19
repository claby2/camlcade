type schedule = Startup | Update
type t = Query.Result.t -> unit

module Registry = struct
  type t' = (schedule, (Query.t * t) list) Hashtbl.t
  type t = t'

  let create : t = Hashtbl.create 0

  let register r schedule query f =
    let current = Hashtbl.find_opt r schedule in
    let current = Option.value current ~default:[] in
    Hashtbl.replace r schedule (current @ [ (query, f) ])

  let fetch r schedule =
    let systems = Hashtbl.find_opt r schedule in
    Option.value systems ~default:[]
end

let run _s = failwith "unimplemented"
