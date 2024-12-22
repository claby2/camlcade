module Vec3 = struct
  type t = { mutable x : float; mutable y : float; mutable z : float }

  let make x y z = { x; y; z }
  let zero = { x = 0.; y = 0.; z = 0. }
  let xyz v = (v.x, v.y, v.z)
  let x v = v.x
  let y v = v.y
  let z v = v.z
  let set_x v x = v.x <- x
  let set_y v y = v.y <- y
  let set_z v z = v.z <- z
  let to_string v = Printf.sprintf "{x=%f, y=%f, z=%f}" v.x v.y v.z
  let sum v = v.x +. v.y +. v.z
  let map f v = { x = f v.x; y = f v.y; z = f v.z }
  let map2 f v1 v2 = { x = f v1.x v2.x; y = f v1.y v2.y; z = f v1.z v2.z }
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
    let lhs = make (v1.y *. v2.z) (v1.z *. v2.x) (v1.x *. v2.y) in
    let rhs = make (v2.y *. v1.z) (v2.z *. v1.x) (v2.x *. v1.y) in
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
