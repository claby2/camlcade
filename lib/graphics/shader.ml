open Tgl4
open Util

let compile src typ =
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid
  else
    let len = get_shader sid Gl.info_log_length in
    let log = get_string len (Gl.get_shader_info_log sid len None) in
    Gl.delete_shader sid;
    Error (`Msg log)

type program = int

let create ~frag ~vert =
  compile vert Gl.vertex_shader >>= fun vert ->
  compile frag Gl.vertex_shader >>= fun frag ->
  let pid = Gl.create_program () in
  Gl.attach_shader pid vert;
  Gl.attach_shader pid frag;
  Gl.delete_shader vert;
  Gl.delete_shader frag;
  Ok pid >>= fun pid -> pid

let pid p = p
let destroy = Gl.delete_program

(* TODO: Do actual phong stuff here *)
let phong_frag =
  "\n\
   #version 330 core\n\n\
   in vec3 worldSpaceNormal;\n\
   in vec3 worldSpacePosition;\n\n\
   out vec4 fragColor;\n\n\
   void main() {\n\
  \    fragColor = vec3(worldSpaceNormal, 1.0);\n\
   }\n"

let phong_vert =
  "\n\
   #version 330 core\n\n\
   layout(location = 0) in vec3 vertexPos;\n\
   layout(location = 1) in vec3 vertexNormal;\n\n\
   uniform mat4 modelMatrix;\n\
   uniform mat4 viewMatrix;\n\
   uniform mat4 projectionMatrix;\n\
   uniform mat3 normalMatrix;\n\n\
   out vec3 worldSpaceNormal;\n\
   out vec3 worldSpacePosition;\n\n\
   void main() {\n\
  \    worldSpacePosition = (modelMatrix * vec4(vertexPos, 1.0)).xyz;\n\
  \    worldSpaceNormal = normalMatrix * normalize(vertexNormal);\n\n\
  \    gl_Position = projectionMatrix * viewMatrix * vec4(worldSpacePosition, \
   1.0);\n\
   }\n"

module Manager = struct
  type state = Empty | Initialized of { phong : program }
  type t = state ref

  let empty () = ref Empty

  let initialize m =
    match !m with
    | Empty ->
        let phong = create ~frag:phong_frag ~vert:phong_vert in
        m := Initialized { phong }
    | Initialized _ -> ()

  let with_phong m view projection f =
    match !m with
    | Empty -> ()
    | Initialized { phong; _ } ->
        let load_matrix4fv pid loc mat =
          let loc = Gl.get_uniform_location pid loc in
          let value = bigarray_create Bigarray.float32 16 in
          let mat = Math.Mat4.to_array mat in
          for i = 0 to 15 do
            value.{i} <- mat.(i / 4).(i mod 4)
          done;
          Gl.uniform_matrix4fv loc 1 false value
        in

        let pid = pid phong in
        Gl.use_program pid;

        load_matrix4fv pid "viewMatrix" view;
        load_matrix4fv pid "projectionMatrix" projection;

        f pid;

        Gl.use_program 0

  let destroy m =
    match !m with Empty -> () | Initialized { phong; _ } -> destroy phong
end
