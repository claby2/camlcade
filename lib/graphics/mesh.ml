module Topology = struct
  type t = PointList | LineList | LineStrip | TriangleList
end

module Attribute = struct
  type t = {
    size : int; (* Number of values per vertex *)
    values : float array;
  }

  let create size values = { size; values }
  let size t = t.size
  let values t = t.values
end

type t = {
  topology : Topology.t;
  attributes : (int, Attribute.t) Hashtbl.t;
  mutable cached_vertex_data : float array option;
}

let create topology =
  { topology; attributes = Hashtbl.create 0; cached_vertex_data = None }

let set_attribute t id attr =
  t.cached_vertex_data <- None;
  Hashtbl.replace t.attributes id attr

let topology t = t.topology
let attributes t = t.attributes

let vertex_data t =
  match t.cached_vertex_data with
  | Some data -> data
  | None ->
      let vertex_size t =
        t.attributes |> Hashtbl.to_seq_values
        |> Seq.fold_left (fun acc attr -> acc + Attribute.size attr) 0
      in
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
      in
      let vertex_data_size t = count_vertices t * vertex_size t in

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

let sphere ?(radius = 0.5) ?(param1 = 2) ?(param2 = 3) () =
  let mesh = create TriangleList in
  let param1 = max param1 2 in
  let param2 = max param2 3 in
  let positions = ref [] in
  let normals = ref [] in
  let add_position v =
    let x, y, z = Math.Vec3.xyz v in
    positions := z :: y :: x :: !positions
  in
  let add_normals v =
    let x, y, z = Math.Vec3.xyz v in
    normals := z :: y :: x :: !normals
  in
  let sphere_coord r theta phi =
    Math.Vec3.make
      (r *. sin phi *. cos theta)
      (r *. cos phi)
      (r *. sin phi *. sin theta)
  in
  let add_tile tl tr bl br =
    let add v =
      add_position v;
      add_normals (Math.Vec3.normalize v)
    in
    add tl;
    add bl;
    add br;

    add tl;
    add br;
    add tr
  in
  let add_wedge theta theta' =
    let step = Float.pi /. float_of_int param1 in
    for i = 0 to param1 - 1 do
      let phi = float_of_int i *. step in
      let phi' = float_of_int (i + 1) *. step in
      let tl = sphere_coord radius theta theta' in
      let tr = sphere_coord radius theta' phi' in
      let bl = sphere_coord radius theta phi in
      let br = sphere_coord radius theta' phi in
      add_tile tl tr bl br
    done
  in
  let step = 2. *. Float.pi /. float_of_int param2 in
  for i = 0 to param2 - 1 do
    let theta = float_of_int i *. step in
    let theta' = float_of_int (i + 1) *. step in
    add_wedge theta theta'
  done;
  set_attribute mesh 0
    (Attribute.create 3 (!positions |> List.rev |> Array.of_list));
  set_attribute mesh 1
    (Attribute.create 3 (!normals |> List.rev |> Array.of_list));
  mesh

(*
let cylinder ?(radius = 0.5) ?(half_height = 0.5) () = failwith "unimplemented"
*)
