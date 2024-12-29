open Util

let frag =
  "\n\
   #version 400 core\n\n\
   in vec3 worldSpaceNormal;\n\
   in vec3 worldSpacePosition;\n\n\
   out vec4 fragColor;\n\n\
   void main() {\n\
  \    fragColor = vec4(abs(worldSpaceNormal), 1.0);\n\
   }\n"

let vert =
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

module C = Ecs.Component.Make (struct
  type inner = unit
end)

let query q =
  let cameras = q (Ecs.Query.create [ Ecs.Query.Required Camera.Dim3.C.id ]) in
  let entities =
    q
      (Ecs.Query.create ~filter:(Ecs.Query.Filter.With C.id)
         [ Ecs.Query.Required Mesh3d.C.id; Ecs.Query.Optional Transform.C.id ])
  in
  ( cameras
    |> Ecs.Query.Result.map (function
         | [ c ] -> Ecs.Component.unpack (module Camera.Dim3.C) c
         | _ -> assert false),
    entities
    |> Ecs.Query.Result.map (function
         | [ m; t ] ->
             ( Ecs.Component.unpack (module Mesh3d.C) m,
               Ecs.Component.unpack_opt (module Transform.C) t )
         | _ -> assert false) )

let render ?(transform = Transform.identity ()) pid camera mesh3d =
  load_matrix4fv (Camera.Dim3.view camera) pid "viewMatrix";
  load_matrix4fv (Camera.Dim3.projection camera) pid "projectionMatrix";

  let transform = Transform.compute_matrix transform in
  let normal_matrix =
    Math.Mat3.inv (Math.Mat3.transpose (Math.Mat3.of_m4 transform))
  in

  load_matrix4fv transform pid "modelMatrix";
  load_matrix3fv normal_matrix pid "normalMatrix";

  Mesh3d.draw mesh3d
