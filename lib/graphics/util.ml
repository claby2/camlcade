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
