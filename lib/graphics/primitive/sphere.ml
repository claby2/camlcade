open Util

type config = {
  radius : float;
  param1 : int;
  param2 : int;
  positions : float list ref;
  normals : float list ref;
}

let add_tile c tl tr bl br =
  let add v =
    add_vec3 c.positions v;
    add_vec3 c.normals (Math.Vec3.normalize v)
  in
  add tl;
  add bl;
  add br;

  add tl;
  add br;
  add tr

let add_wedge c theta theta' =
  let step = Float.pi /. float_of_int c.param1 in
  for i = 0 to c.param1 - 1 do
    let phi = float_of_int i *. step in
    let phi' = float_of_int (i + 1) *. step in
    let spherical = Math.Vec3.spherical c.radius in
    let tl = spherical theta phi' in
    let tr = spherical theta' phi' in
    let bl = spherical theta phi in
    let br = spherical theta' phi in
    add_tile c tl tr bl br
  done

let construct c =
  let step = 2. *. Float.pi /. float_of_int c.param2 in
  for i = 0 to c.param2 - 1 do
    let theta = float_of_int i *. step in
    let theta' = float_of_int (i + 1) *. step in
    add_wedge c theta theta'
  done;
  Shape.{ positions = !(c.positions); normals = !(c.normals) }

let create ?(radius = 0.5) ?(param1 = 2) ?(param2 = 3) () =
  let param1 = max param1 2 in
  let param2 = max param2 3 in
  construct { radius; param1; param2; positions = ref []; normals = ref [] }
