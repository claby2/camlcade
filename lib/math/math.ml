module Vec3 = struct
  type t = (float * float * float) ref

  let make x y z = ref (x, y, z)
  let zero = make 0. 0. 0.
  let xyz v = !v
  let x v = xyz v |> fun (x, _, _) -> x
  let y v = xyz v |> fun (_, y, _) -> y
  let z v = xyz v |> fun (_, _, z) -> z

  let set_x v x =
    let _, y, z = !v in
    v := (x, y, z)

  let set_y v y =
    let x, _, z = !v in
    v := (x, y, z)

  let set_z v z =
    let x, y, _ = !v in
    v := (x, y, z)

  let to_string v = Printf.sprintf "{x=%f, y=%f, z=%f}" (x v) (y v) (z v)

  let sum v =
    let x, y, z = !v in
    x +. y +. z

  let map f v = make (f (x v)) (f (y v)) (f (z v))
  let map2 f v1 v2 = make (f (x v1) (x v2)) (f (y v1) (y v2)) (f (z v1) (z v2))
  let ( + ) = map2 ( +. )
  let ( - ) = map2 ( -. )
  let ( * ) = map2 ( *. )
  let ( / ) = map2 ( /. )
  let dot v1 v2 = v1 * v2 |> sum
  let length v = sqrt (dot v v)
  let length_squared v = dot v v
  let length_recip v = 1. /. length v

  let normalize v =
    let lr = length_recip v in
    map (fun x -> x *. lr) v

  let neg = map (fun x -> -.x)
  let scale f = map (fun x -> x *. f)

  let cross v1 v2 =
    let x1, y1, z1 = !v1 in
    let x2, y2, z2 = !v2 in
    let lhs = make (y1 *. z2) (z1 *. x2) (x1 *. y2) in
    let rhs = make (y2 *. z1) (z2 *. x1) (x2 *. y1) in
    lhs - rhs
end

module type Matrix = sig
  type t

  val create : ?default:float -> unit -> t
  val identity : t
  val from_array : float array array -> t
  val to_array : t -> float array array
  val inverse : t -> t
  val transpose : t -> t
  val ( * ) : t -> t -> t
end

open Lacaml.S

module MakeMatN (B : sig
  val dim : int
end) : Matrix = struct
  type t = Mat.t

  let create ?(default = 0.) () = Mat.make B.dim B.dim default
  let identity = Mat.identity B.dim
  let from_array a = Mat.of_array a
  let to_array m = Mat.to_array m

  let inverse m =
    (* "Out-place" *)
    let m' = from_array (to_array m) in
    getri m';
    m'

  let transpose m = Mat.transpose_copy m
  let ( * ) m1 m2 = gemm m1 m2
end

module Mat3 = MakeMatN (struct
  let dim = 3
end)

module Mat4 = MakeMatN (struct
  let dim = 4
end)
