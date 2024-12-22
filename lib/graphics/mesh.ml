module Topology = struct
  type t = PointList | LineList | LineStrip | TriangleList
end

module Attribute = struct
  type t = {
    size : int; (* Number of values per vertex *)
    values : float array;
  }

  let size t = t.size
  let values t = t.values
end

type t = {
  topology : Topology.t;
  attributes : (int, Attribute.t) Hashtbl.t;
  mutable cached_vertex_data : float array option;
}

let set_attribute t id attr =
  t.cached_vertex_data <- None;
  Hashtbl.replace t.attributes id attr

let topology t = t.topology
let attributes t = t.attributes

let vertex_size t =
  t.attributes |> Hashtbl.to_seq_values
  |> Seq.fold_left (fun acc attr -> acc + Attribute.size attr) 0

let count_vertices t =
  if Hashtbl.length t.attributes = 0 then 0
  else
    t.attributes |> Hashtbl.to_seq_values
    |> Seq.fold_left
         (fun acc attr ->
           let num_values = Array.length (Attribute.values attr) in
           let size = Attribute.size attr in
           min acc (num_values / size))
         Int.max_int

let vertex_data_size t = count_vertices t * vertex_size t

let vertex_data t =
  match t.cached_vertex_data with
  | Some data -> data
  | None ->
      let data = Array.make (vertex_data_size t) 0. in
      let vsize = vertex_size t in
      let attribute_offset = ref 0 in

      let load_attribute attr =
        Attribute.values attr |> Array.to_seq
        |> Seq.iteri (fun i v ->
               let local_offset = i mod Attribute.size attr in
               let vertex_index = i / Attribute.size attr in
               let offset =
                 local_offset + (vertex_index * vsize) + !attribute_offset
               in
               data.(offset) <- v);

        attribute_offset := !attribute_offset + Attribute.size attr
      in

      t.attributes |> Hashtbl.to_seq |> List.of_seq |> List.sort compare
      |> List.map snd |> List.iter load_attribute;
      t.cached_vertex_data <- Some data;
      data
