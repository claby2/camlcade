module Vec3 = struct
  include Gg.V3

  let normalize t = Gg.V3.smul (1. /. Gg.V3.norm t) t
end

module Vec4 = struct
  include Gg.V4

  let normalize t = Gg.V4.smul (1. /. Gg.V4.norm t) t
end

module Mat3 = struct
  include Gg.M3

  let to_list t =
    [
      Gg.M3.e00 t;
      Gg.M3.e10 t;
      Gg.M3.e20 t;
      Gg.M3.e01 t;
      Gg.M3.e11 t;
      Gg.M3.e21 t;
      Gg.M3.e02 t;
      Gg.M3.e12 t;
      Gg.M3.e22 t;
    ]
end

module Mat4 = struct
  include Gg.M4

  let to_list t =
    [
      Gg.M4.e00 t;
      Gg.M4.e10 t;
      Gg.M4.e20 t;
      Gg.M4.e30 t;
      Gg.M4.e01 t;
      Gg.M4.e11 t;
      Gg.M4.e21 t;
      Gg.M4.e31 t;
      Gg.M4.e02 t;
      Gg.M4.e12 t;
      Gg.M4.e22 t;
      Gg.M4.e32 t;
      Gg.M4.e03 t;
      Gg.M4.e13 t;
      Gg.M4.e23 t;
      Gg.M4.e33 t;
    ]
end
