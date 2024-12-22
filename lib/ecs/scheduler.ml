type schedule = Startup | Update | Last

type 'a t = {
  mutable startup : 'a list;
  mutable update : 'a list;
  mutable last : 'a list;
}

let create () = { startup = []; update = []; last = [] }

let register s schedule v =
  match schedule with
  | Startup -> s.startup <- v :: s.startup
  | Update -> s.update <- v :: s.update
  | Last -> s.last <- v :: s.last

let fetch s schedule =
  (match schedule with
  | Startup -> s.startup
  | Update -> s.update
  | Last -> s.last)
  |> List.rev
