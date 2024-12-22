module Vec3 = struct
  type t = { mutable x : float; mutable y : float; mutable z : float }

  let make x y z = { x; y; z }
  let zero = { x = 0.; y = 0.; z = 0. }
  let x v = v.x
  let y v = v.y
  let z v = v.z
  let set_x v x = v.x <- x
  let set_y v y = v.y <- y
  let set_z v z = v.z <- z
  let to_string v = Printf.sprintf "{x=%f, y=%f, z=%f}" v.x v.y v.z

  let make_bin_op f v1 v2 =
    { x = f v1.x v2.x; y = f v1.y v2.y; z = f v1.z v2.z }

  let ( + ) = make_bin_op ( +. )
  let ( - ) = make_bin_op ( -. )
  let ( * ) = make_bin_op ( *. )
  let ( / ) = make_bin_op ( /. )
end
