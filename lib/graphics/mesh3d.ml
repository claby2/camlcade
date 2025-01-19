open Tgl4
open Util

type mesh3d =
  | Staged of { mesh : Vertex_mesh.t }
  | Initialized of { mesh : Vertex_mesh.t; vao : int; vbo : int }
  | Installed of { mesh : Vertex_mesh.t; vao : int; vbo : int }

type t = mesh3d ref

let of_vertex_mesh mesh = ref (Staged { mesh })

let vertex_mesh t =
  match !t with
  | Staged { mesh } | Initialized { mesh; _ } -> mesh
  | Installed { mesh; _ } -> mesh

let initialize t =
  match !t with
  | Staged { mesh } ->
      let vao = get_int (Gl.gen_vertex_arrays 1) in
      let vbo = get_int (Gl.gen_buffers 1) in
      t := Initialized { mesh; vao; vbo }
  | Initialized _ -> ()
  | Installed _ -> ()

let with_vao vao f =
  Gl.bind_vertex_array vao;
  f ();
  Gl.bind_vertex_array 0

let with_vbo vbo f =
  Gl.bind_buffer Gl.array_buffer vbo;
  f ();
  Gl.bind_buffer Gl.array_buffer 0

let rec install t =
  let aux mesh vao vbo =
    with_vao vao (fun () ->
        with_vbo vbo (fun () ->
            let vertex_data = Vertex_mesh.data mesh in
            let varray =
              bigarray_create Bigarray.float32 (Array.length vertex_data)
            in
            vertex_data |> Array.iteri (fun i v -> varray.{i} <- v);
            Gl.buffer_data Gl.array_buffer
              (Gl.bigarray_byte_size varray)
              (Some varray) Gl.static_draw;

            let size_of_float = 4 in
            let stride = Vertex_mesh.vertex_size mesh * size_of_float in
            let offset = ref 0 in
            Vertex_mesh.attributes mesh
            |> Hashtbl.to_seq |> List.of_seq |> List.sort compare
            |> List.iter (fun (index, size) ->
                   Gl.enable_vertex_attrib_array index;
                   Gl.vertex_attrib_pointer index size Gl.float false stride
                     (`Offset (!offset * size_of_float));
                   offset := !offset + size)))
  in
  match !t with
  | Staged _ ->
      initialize t;
      install t
  | Initialized { mesh; vao; vbo } | Installed { mesh; vao; vbo } ->
      (* Here we still install even if already installed in case the mesh
         has changed *)
      aux mesh vao vbo;
      t := Installed { mesh; vao; vbo }

let rec draw t =
  match !t with
  | Installed { mesh; vao; _ } ->
      with_vao vao (fun () ->
          let mode =
            match Vertex_mesh.topology mesh with
            | Vertex_mesh.TriangleList -> Gl.triangles
          in
          Gl.draw_arrays mode 0 (Vertex_mesh.count_vertices mesh))
  | Initialized _ ->
      install t;
      draw t
  | Staged _ ->
      initialize t;
      draw t

let destroy t =
  match !t with
  | Initialized { vbo; vao; _ } | Installed { vbo; vao; _ } ->
      set_int (Gl.delete_buffers 1) vbo;
      set_int (Gl.delete_vertex_arrays 1) vao
  | _ -> ()

module C = Ecs.Component.Make (struct
  type inner = t
end)
