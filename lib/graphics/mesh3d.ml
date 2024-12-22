open Tgl4
open Util

module T = struct
  type t = { mesh : Mesh.t; mutable vao : int; mutable vbo : int }

  let from_mesh mesh = { mesh; vao = 0; vbo = 0 }
  let mesh t = t.mesh

  let initialize t =
    t.vao <- get_int (Gl.gen_vertex_arrays 1);
    t.vbo <- get_int (Gl.gen_buffers 1)

  let with_vao t f =
    Gl.bind_vertex_array t.vao;
    f ();
    Gl.bind_vertex_array 0

  let draw t =
    with_vao t (fun _ ->
        Gl.draw_arrays Gl.triangles 0 (Mesh.count_vertices t.mesh))

  let install_vbo t =
    with_vao t (fun () ->
        Gl.bind_buffer Gl.array_buffer t.vbo;

        (* TODO: OPTIMIZATION! Only do this if the mesh has changed? How to do this? *)
        let vdata = Mesh.vertex_data t.mesh in
        let varray = bigarray_create Bigarray.float32 (Array.length vdata) in
        vdata |> Array.iteri (fun i v -> varray.{i} <- v);
        Gl.buffer_data Gl.array_buffer
          (Gl.bigarray_byte_size varray)
          (Some varray) Gl.static_draw;

        let offset = ref 0 in
        let bind_attrib loc attr =
          Gl.enable_vertex_attrib_array loc;
          let dim = Mesh.Attribute.size attr in
          (* TODO: Here we assume attribute values are all floats *)
          Gl.vertex_attrib_pointer loc dim Gl.float false 0 (`Offset !offset);
          offset := !offset + dim
        in
        Mesh.attributes t.mesh |> Hashtbl.to_seq_values |> Seq.iteri bind_attrib;

        Gl.bind_buffer Gl.array_buffer 0)

  let destroy t =
    set_int (Gl.delete_buffers 1) t.vbo;
    set_int (Gl.delete_vertex_arrays 1) t.vao
end

module C = Ecs.Component.Make (T)
