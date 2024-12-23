open Math

let _print_mat4 a =
  Printf.printf "[|";
  Array.iter
    (fun a ->
      Printf.printf "[|";
      Array.iter (fun f -> Printf.printf "%f, " f) a;
      Printf.printf "|]\n")
    (Mat4.to_array a);
  Printf.printf "|]\n"

let _rotate_mat4 m =
  let a = Mat4.to_array m in
  let new_a = Array.init 4 (fun _ -> Array.make 4 0.) in
  for i = 0 to 3 do
    for j = 0 to 3 do
      new_a.(i).(j) <- a.(j).(i)
    done
  done;
  Mat4.from_array a

let test_mat_transpose () =
  let m =
    Mat4.from_array
      [|
        [| 1.; 2.; 3.; 4. |];
        [| 5.; 6.; 7.; 8. |];
        [| 9.; 1.; 2.; 3. |];
        [| 4.; 5.; 6.; 7. |];
      |]
  in
  assert (m = Mat4.transpose (Mat4.transpose m))

let test_mat_mult () =
  let m1 =
    Mat3.from_array [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |]; [| 7.; 8.; 9. |] |]
  in
  let m2 =
    Mat3.from_array [| [| 9.; 8.; 7. |]; [| 6.; 5.; 4. |]; [| 3.; 2.; 1. |] |]
  in
  let m_product = Mat3.(m1 * m2) in
  (* Expected product:
     [ (1*9 + 2*6 + 3*3), (1*8 + 2*5 + 3*2), (1*7 + 2*4 + 3*1) ] = [ 30, 24, 18 ]
     [ (4*9 + 5*6 + 6*3), (4*8 + 5*5 + 6*2), (4*7 + 5*4 + 6*1) ] = [ 84, 69, 54 ]
     [ (7*9 + 8*6 + 9*3), (7*8 + 8*5 + 9*2), (7*7 + 8*4 + 9*1) ] = [ 138, 114, 90 ]
  *)
  let expected =
    Mat3.from_array
      [| [| 30.; 24.; 18. |]; [| 84.; 69.; 54. |]; [| 138.; 114.; 90. |] |]
  in
  assert (m_product = expected)

let () =
  test_mat_transpose ();
  test_mat_mult ()
