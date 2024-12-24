open Tgl4
open Util

module T = struct
  type t = {
    mesh : Vertex_mesh.t;
    mutable vao : int option;
    mutable vbo : int option;
  }

  let from_vertex_mesh mesh = { mesh; vao = None; vbo = None }

  let from_primitive primitive =
    Vertex_mesh.of_primitive primitive |> from_vertex_mesh

  let vertex_mesh t = t.mesh

  let initialize t =
    match (t.vao, t.vbo) with
    | Some _, Some _ -> ()
    | _ ->
        t.vao <- Some (get_int (Gl.gen_vertex_arrays 1));
        t.vbo <- Some (get_int (Gl.gen_buffers 1))

  let with_vao t f =
    match t.vao with
    | None -> invalid_arg "Mesh3d.with_vao: VAO not initialized"
    | Some vao ->
        Gl.bind_vertex_array vao;
        f ();
        Gl.bind_vertex_array 0

  let draw t =
    with_vao t (fun _ ->
        let mode =
          match Vertex_mesh.topology t.mesh with
          | Vertex_mesh.TriangleList -> Gl.triangles
        in
        Gl.draw_arrays mode 0 (Vertex_mesh.count_vertices t.mesh))

  let install_vbo t =
    match t.vbo with
    | None -> invalid_arg "Mesh3d.install_vbo: VBO not initialized"
    | Some vbo ->
        with_vao t (fun () ->
            Gl.bind_buffer Gl.array_buffer vbo;

            let vertex_data = Vertex_mesh.vertex_data t.mesh in
            let varray =
              bigarray_create Bigarray.float32 (Array.length vertex_data)
            in
            vertex_data |> Array.iteri (fun i v -> varray.{i} <- v);
            Gl.buffer_data Gl.array_buffer
              (Gl.bigarray_byte_size varray)
              (Some varray) Gl.static_draw;

            let size_of_float = 4 in
            let stride = Vertex_mesh.vertex_size t.mesh * size_of_float in
            let attribute_info = Vertex_mesh.attribute_info t.mesh in
            let offset = ref 0 in
            attribute_info
            |> Seq.iter (fun info ->
                   let { Vertex_mesh.Attribute.index; size } = info in
                   Gl.enable_vertex_attrib_array index;
                   Gl.vertex_attrib_pointer index size Gl.float false stride
                     (`Offset (!offset * size_of_float));
                   offset := !offset + size);

            Gl.bind_buffer Gl.array_buffer 0)

  let destroy t =
    match (t.vbo, t.vao) with
    | Some vbo, Some vao ->
        set_int (Gl.delete_buffers 1) vbo;
        set_int (Gl.delete_vertex_arrays 1) vao
    | _ -> ()
end

module C = Ecs.Component.Make (T)
