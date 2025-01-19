open Tgl4

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f ->
    f a;
    Int32.to_int a.{0}

let set_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f i ->
    a.{0} <- Int32.of_int i;
    f a

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a;
  Gl.string_of_bigarray a

let ( >>= ) x f =
  match x with Ok v -> f v | Error (`Msg msg) -> raise (Failure msg)

let load_matrixnfv n mat f pid loc =
  let loc = Gl.get_uniform_location pid loc in
  let value = bigarray_create Bigarray.float32 (n * n) in
  mat |> List.iteri (fun i x -> value.{i} <- x);
  f loc 1 false value

let load_matrix3fv mat =
  load_matrixnfv 3 (Math.Mat3.to_list mat) Gl.uniform_matrix3fv

let load_matrix4fv mat =
  load_matrixnfv 4 (Math.Mat4.to_list mat) Gl.uniform_matrix4fv

let load_uniform1i i pid loc = Gl.uniform1i (Gl.get_uniform_location pid loc) i
let load_uniform1f i pid loc = Gl.uniform1f (Gl.get_uniform_location pid loc) i

let load_uniform3fv v pid loc =
  let loc = Gl.get_uniform_location pid loc in
  let value = bigarray_create Bigarray.float32 3 in
  value.{0} <- Math.Vec3.x v;
  value.{1} <- Math.Vec3.y v;
  value.{2} <- Math.Vec3.z v;
  Gl.uniform3fv loc 1 value

let check_gl_error () =
  let error = ref (Gl.get_error ()) in
  let errored = ref false in
  while !error != Gl.no_error do
    print_endline (Printf.sprintf "%d\n" !error);
    error := Gl.get_error ();
    errored := true
  done;
  if !errored then failwith "shortcircuit in print_gl_error"

let prepend_vec3 l v =
  let x, y, z = Math.Vec3.to_tuple v in
  l := x :: y :: z :: !l
