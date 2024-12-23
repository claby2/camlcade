open Graphics.Vertex_mesh

let test_empty () =
  let v = create TriangleList in
  assert (topology v = TriangleList);
  assert (count_vertices v = 0);
  Array.iteri (fun i v -> Printf.printf "%d: %f\n" i v) (vertex_data v);
  assert (vertex_data v = [||])

let test_vertex_data () =
  let v =
    create TriangleList
    |> with_attribute
         (Attribute.create ~index:0 ~size:3 [| 1.; 2.; 3.; 4.; 5.; 6. |])
    |> with_attribute
         (Attribute.create ~index:2 ~size:1 [| 11.; 12.; 13.; 14. |])
    |> with_attribute (Attribute.create ~index:1 ~size:2 [| 7.; 8.; 9.; 10. |])
  in
  let vdata = vertex_data v in
  assert (count_vertices v = 2);
  assert (
    attribute_info v |> List.of_seq
    = [
        { index = 0; size = 3 };
        { index = 1; size = 2 };
        { index = 2; size = 1 };
      ]);
  assert (vdata = [| 1.; 2.; 3.; 7.; 8.; 11.; 4.; 5.; 6.; 9.; 10.; 12. |]);
  assert (vdata = vertex_data v)

let test_vertex_data_changed () =
  let v1 = create TriangleList in
  assert (vertex_data v1 = [||]);

  let v2 =
    with_attribute
      (Attribute.create ~index:0 ~size:3 [| 1.; 2.; 3.; 4.; 5.; 6. |])
      v1
  in
  assert (vertex_data v2 = [| 1.; 2.; 3.; 4.; 5.; 6. |]);
  assert (vertex_data v2 <> vertex_data v1)

let () =
  test_empty ();
  test_vertex_data ();
  test_vertex_data_changed ()
