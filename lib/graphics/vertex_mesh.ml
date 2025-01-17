type topology = TriangleList

type t = {
  mutable topology : topology;
  attributes : (int, int) Hashtbl.t;
  mutable data : float array;
}

let create ?(topology = TriangleList) () =
  { topology; attributes = Hashtbl.create 0; data = [||] }

let topology t = t.topology
let set_topology t topology = t.topology <- topology
let attributes t = t.attributes
let set_attribute t index size = Hashtbl.replace t.attributes index size
let data t = t.data
let set_data t data = t.data <- data
let vertex_size t = Hashtbl.fold (fun _ size acc -> acc + size) t.attributes 0

let count_vertices t =
  if Hashtbl.length t.attributes = 0 then 0
  else Array.length t.data / vertex_size t
