open Containers

type 'a t = {
  dense : 'a Vector.vector;
  sparse : int Sparse_array.t;
  indices : int Vector.vector;
}

let create () =
  {
    dense = Vector.create ();
    sparse = Sparse_array.create ();
    indices = Vector.create ();
  }

let length t = Vector.length t.dense
let contains t i = Sparse_array.contains t.sparse i

let get t i =
  Sparse_array.get t.sparse i
  |> Option.map (Array.get (Vector.unsafe_get_array t.dense))

let set t i v =
  match Sparse_array.get t.sparse i with
  | Some j -> Vector.set t.dense j v
  | None ->
      Sparse_array.set t.sparse i (Vector.length t.dense);
      Vector.push t.indices i;
      Vector.push t.dense v

let remove t i =
  Sparse_array.remove t.sparse i
  |> Option.map (fun j ->
         let last = Vector.length t.dense - 1 in
         let value = Vector.get t.dense j in
         Vector.remove_unordered t.dense j;
         Vector.remove_unordered t.indices j;
         (if j < last then
            let swapped = Vector.get t.indices j in
            Sparse_array.set t.sparse swapped j);
         value)
