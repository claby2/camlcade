open Tgl4
open Util

(* TODO: Do actual phong stuff here *)
let phong_frag =
  "\n\
   #version 400 core\n\n\
   in vec3 worldSpaceNormal;\n\
   in vec3 worldSpacePosition;\n\n\
   out vec4 fragColor;\n\n\
   void main() {\n\
  \    fragColor = vec4(abs(worldSpaceNormal), 1.0);\n\
   }\n"

let phong_vert =
  "\n\
   #version 400 core\n\n\
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

type tag = Phong | Custom of string

type program =
  | Staged of { frag : string; vert : string; t : tag }
  | Initialized of { pid : int; t : tag }
  | Destroyed

type t = program ref

let create ~frag ~vert name = ref (Staged { frag; vert; t = Custom name })
let phong = ref (Staged { frag = phong_frag; vert = phong_vert; t = Phong })

let tag_opt s =
  match !s with
  | Staged { t; _ } | Initialized { t; _ } -> Some t
  | Destroyed -> None

let initialize s =
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
  in
  match !s with
  | Staged { frag; vert; t } ->
      compile vert Gl.vertex_shader >>= fun vert ->
      compile frag Gl.fragment_shader >>= fun frag ->
      let pid = Gl.create_program () in
      Gl.attach_shader pid vert;
      Gl.attach_shader pid frag;
      Gl.link_program pid;
      Gl.delete_shader vert;
      Gl.delete_shader frag;
      Ok pid >>= fun pid -> s := Initialized { pid; t }
  | Initialized _ -> ()
  | Destroyed -> failwith "attempt to initialize destroyed shader"

let with_shader s f =
  match !s with
  | Initialized { pid; _ } ->
      Gl.use_program pid;
      f pid;
      Gl.use_program 0
  | _ -> failwith "attempt to use uninitialized shader"

let destroy s =
  match !s with
  | Initialized { pid; _ } ->
      Gl.delete_program pid;
      s := Destroyed
  | _ -> ()

module C = Ecs.Component.Make (struct
  type inner = t
end)
