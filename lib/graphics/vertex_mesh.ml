(*type attribute = {*)
(**)
(**)
(*}*)

(* What do we need to keep track of for the call to VertexAttribPointer?
   index = check
   size = check
   type = check from attribute_values
   stride = sum of all attribute sizes times by size in bits
   offset = this requires iterating in a sorted order by index, use a set?

   hash table then sort

   even if we use a set, it's still O(logN) to insert each attribute. then O(NlogN) to insert N attributes, but this might be better because this is done just at the start most likely rather than at runtime

   How do we cache these results?
   1. updated tag on the vertex manager, user must call update
   2. only allow inserting attribute, can do if we use set
*)

module Attribute = struct
  type info = { index : int; size : int }
  type t = { values : float array; info : info }

  let create ~index ~size values = { values; info = { index; size } }
  let index t = t.info.index
  let size t = t.info.size
  let info t = t.info
  let values t = t.values
  let compare a b = compare a.info.index b.info.index
end

module AttributeSet = Set.Make (Attribute)

type topology = TriangleList

type t = {
  topology : topology;
  attributes : AttributeSet.t;
  mutable cached_vertex_data : float array option;
}

let create topology =
  { topology; attributes = AttributeSet.empty; cached_vertex_data = None }

let topology t = t.topology
let with_topology topology t = { t with topology }

let with_attribute attr t =
  {
    t with
    attributes = AttributeSet.add attr t.attributes;
    cached_vertex_data = None;
  }

let of_primitive primitive =
  let t = create TriangleList in
  let position_attr =
    Attribute.create ~index:0 ~size:3
      (Array.of_list (Primitive.positions primitive))
  in
  let normal_attr =
    Attribute.create ~index:1 ~size:3
      (Array.of_list (Primitive.normals primitive))
  in
  t |> with_attribute position_attr |> with_attribute normal_attr

let count_vertices t =
  if AttributeSet.is_empty t.attributes then 0
  else
    t.attributes |> AttributeSet.to_seq
    |> Seq.fold_left
         (fun acc attr ->
           let num_values = Array.length (Attribute.values attr) in
           let size = Attribute.size attr in
           min acc (num_values / size))
         Int.max_int

let vertex_size t =
  t.attributes |> AttributeSet.to_seq
  |> Seq.fold_left (fun acc attr -> acc + Attribute.size attr) 0

let attribute_info t =
  t.attributes |> AttributeSet.to_seq |> Seq.map Attribute.info

let vertex_data t =
  match t.cached_vertex_data with
  | Some data -> data
  | None ->
      let attributes = t.attributes |> AttributeSet.to_seq in
      let number_of_vertices = count_vertices t in
      let vertex_size = vertex_size t in
      let vertex_data_size = number_of_vertices * vertex_size in
      let data = Array.make vertex_data_size 0. in

      let offset = ref 0 in
      let write_attribute attr =
        let values = Attribute.values attr in
        (try
           values |> Array.to_seq
           |> Seq.iteri (fun i v ->
                  let vertex_number = i / Attribute.size attr in
                  let local_offset = (vertex_number * vertex_size) + !offset in
                  let index = local_offset + (i mod Attribute.size attr) in
                  if index >= vertex_data_size then
                    raise (Invalid_argument "too many values for vertex");
                  data.(index) <- v)
         with Invalid_argument _ -> ());
        offset := !offset + Attribute.size attr
      in

      attributes |> Seq.iter write_attribute;
      t.cached_vertex_data <- Some data;
      data
