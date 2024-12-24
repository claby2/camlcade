open Core_bench
open Math

let t1 =
  Bench.Test.create ~name:"mat4 operations" (fun () ->
      let m =
        Mat4.of_rows (Math.Vec4.v 1. 2. 3. 4.) (Math.Vec4.v 5. 6. 7. 8.)
          (Math.Vec4.v 9. 10. 11. 12.)
          (Math.Vec4.v 13. 14. 15. 16.)
      in
      ignore (Mat4.inv m);
      ignore (Mat4.transpose m);
      ignore (Mat4.mul m m))

let tests = [ t1 ]
let command = Bench.make_command tests
