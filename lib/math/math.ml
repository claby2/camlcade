module Vec3 = struct
  type t = float * float * float

  let make x y z = (x, y, z)
  let zero = (0., 0., 0.)
  let x (x, _, _) = x
  let y (_, y, _) = y
  let z (_, _, z) = z
end
