type schedule = Startup | Update
type 'a t = { mutable startup : 'a list; mutable update : 'a list }

let create () = { startup = []; update = [] }

let register s schedule v =
  match schedule with
  | Startup -> s.startup <- v :: s.startup
  | Update -> s.update <- v :: s.update

let fetch s schedule =
  (match schedule with Startup -> s.startup | Update -> s.update) |> List.rev
