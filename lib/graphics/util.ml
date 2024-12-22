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

let load_matrixfv n mat pid loc =
  let loc = Gl.get_uniform_location pid loc in
  let value = bigarray_create Bigarray.float32 (n * n) in
  for i = 0 to (n * n) - 1 do
    value.{i} <- mat.(i / n).(i mod n)
  done;
  Gl.uniform_matrix4fv loc 1 false value

let load_matrix3fv mat = load_matrixfv 3 (Math.Mat3.to_array mat)
let load_matrix4fv mat = load_matrixfv 4 (Math.Mat4.to_array mat)

let print_gl_error () =
  let error = ref (Gl.get_error ()) in
  let errored = ref false in
  while !error != Gl.no_error do
    print_endline (Printf.sprintf "%d\n" !error);
    error := Gl.get_error ();
    errored := true
  done;
  if !errored then failwith "shortcircuit in print_gl_error"
