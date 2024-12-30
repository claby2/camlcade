open Util

type config = {
  param1 : int;
  half_size : Math.Vec3.t;
  positions : float list ref;
  normals : float list ref;
}

let add_tile c topl topr botl botr =
  add_vec3 c.positions topl;
  add_vec3 c.normals Math.Vec3.(normalize (cross (botl - topl) (botr - topl)));
  add_vec3 c.positions botl;
  add_vec3 c.normals Math.Vec3.(normalize (cross (botr - botl) (topl - botl)));
  add_vec3 c.positions botr;
  add_vec3 c.normals Math.Vec3.(normalize (cross (topl - botr) (botl - botr)));

  add_vec3 c.positions topl;
  add_vec3 c.normals Math.Vec3.(normalize (cross (botr - topl) (topr - topl)));
  add_vec3 c.positions botr;
  add_vec3 c.normals Math.Vec3.(normalize (cross (topr - botr) (topl - botr)));
  add_vec3 c.positions topr;
  add_vec3 c.normals Math.Vec3.(normalize (cross (topl - topr) (botr - topr)))

let add_face c botl topl topr =
  let size = 1. /. float_of_int c.param1 in
  let col_delta = Math.Vec3.(smul size (topr - topl)) in
  let row_delta = Math.Vec3.(smul size (botl - topl)) in
  for col = 0 to c.param1 - 1 do
    for row = 0 to c.param1 - 1 do
      let col = float_of_int col in
      let row = float_of_int row in
      let tl = Math.Vec3.(topl + smul col col_delta + smul row row_delta) in
      let tr = Math.Vec3.(tl + col_delta) in
      let bl = Math.Vec3.(tl + row_delta) in
      let br = Math.Vec3.(bl + col_delta) in
      add_tile c tl tr bl br
    done
  done

let construct c =
  let x, y, z = Math.Vec3.to_tuple c.half_size in
  let add = add_face c in
  let v = Math.Vec3.v in
  add (v (-.x) (-.y) z) (v (-.x) y z) (v x y z);
  add (v (-.x) (-.y) (-.z)) (v (-.x) y (-.z)) (v (-.x) y z);
  add (v x (-.y) z) (v x y z) (v x y (-.z));
  add (v x (-.y) (-.z)) (v x y (-.z)) (v (-.x) y (-.z));
  add (v x y (-.z)) (v x y z) (v (-.x) y z);
  add (v x (-.y) z) (v x (-.y) (-.z)) (v (-.x) (-.y) (-.z));
  Shape.{ positions = !(c.positions); normals = !(c.normals) }

let create ?(x_length = 1.) ?(y_length = 1.) ?(z_length = 1.) ?(param1 = 2) () =
  let param1 = max param1 1 in
  let half_size = Math.Vec3.(smul 0.5 (v x_length y_length z_length)) in
  construct { half_size; param1; positions = ref []; normals = ref [] }
