open Util

let frag = "
#version 400 core
in vec3 worldSpaceNormal;
in vec3 worldSpacePosition;
out vec4 fragColor;
void main() {
  fragColor = vec4(abs(worldSpaceNormal), 1.0);
}" [@@ocamlformat "disable"]

let vert = "
#version 400 core
layout(location = 0) in vec3 vertexPos;
layout(location = 1) in vec3 vertexNormal;
uniform mat4 modelMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;
uniform mat3 normalMatrix;
out vec3 worldSpaceNormal;
out vec3 worldSpacePosition;
void main() {
  worldSpacePosition = (modelMatrix * vec4(vertexPos, 1.0)).xyz;
  worldSpaceNormal = normalMatrix * normalize(vertexNormal);
  gl_Position = projectionMatrix * viewMatrix * vec4(worldSpacePosition, 1.0);
}" [@@ocamlformat "disable"]

module C = Ecs.Component.Make (struct
  type inner = unit
end)

let query w =
  let open Ecs in
  let cameras =
    World.query ~filter:(Query.Filter.With Camera.Camera3d.C.id) w
      Query.(Req (module Camera.Projection.C) @ Opt (module Transform.C) @ Nil)
  in
  let entities =
    World.query ~filter:(Query.Filter.With C.id) w
      Query.(Req (module Mesh3d.C) @ Opt (module Transform.C) @ Nil)
  in
  ( List.map (fun (_, (c, (t, ()))) -> (c, t)) cameras,
    List.map (fun (_, (m, (t, ()))) -> (m, t)) entities )

let render ?(transform = Transform.identity ())
    ?(camera_transform = Transform.identity ()) pid projection mesh3d =
  let view = Math.Mat4.inv (Transform.compute_matrix camera_transform) in
  load_matrix4fv view pid "viewMatrix";

  let projection = Camera.Projection.project projection in
  load_matrix4fv projection pid "projectionMatrix";

  let transform = Transform.compute_matrix transform in
  let normal_matrix =
    Math.Mat3.inv (Math.Mat3.transpose (Math.Mat3.of_m4 transform))
  in

  load_matrix4fv transform pid "modelMatrix";
  load_matrix3fv normal_matrix pid "normalMatrix";

  Mesh3d.draw mesh3d
