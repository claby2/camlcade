open Util

let frag = "
#version 400 core

in vec3 worldSpaceNormal;
in vec3 worldSpacePosition;

struct Light {
  // 0: Point, 1: Directional, 2: Spot
  int type;

  vec3 color;
  vec3 dir;
  vec3 pos;
  vec3 attenuation;

  float angle;
  float penumbra;
};

struct Material {
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
  float shininess;
};

uniform Material material;
uniform Light lights[128];
uniform int lightCount;
uniform vec3 worldSpaceCamera;

out vec4 fragColor;

vec3 calculateLightDirection(Light light, vec3 pos) {
  switch (light.type) {
    case 0:
    case 2:
      return normalize(light.pos - pos);
    case 1:
    default:
      return normalize(-light.dir);
  }
}

float calculateAttenuation(Light light, float distance) {
  float c1 = light.attenuation.x;
  float c2 = light.attenuation.y;
  float c3 = light.attenuation.z;
  return min(1.0f, 1.0f / (c1 + c2 * distance + c3 * distance * distance));
}

float calculateSpotLightFactor(Light light, vec3 lightDirection) {
  float x = acos(dot(normalize(-light.dir),
                               normalize(lightDirection)));
  float outer = light.angle;
  float inner = outer - light.penumbra;
  if (x <= inner) {
    return 1.0f;
  } else if (x > inner && x <= outer) {
    float factor = (x - inner) / (outer - inner);
    return 1.0f - (-2.0f * pow(factor, 3) + 3.0f * pow(factor, 2));
  } else {
    return 0.0f;
  }
}

void main() {
  // TODO: Make this configurable ?
  float ka = 1.0; float kd = 1.0; float ks = 1.0;

  fragColor = vec4(0.0);

  // Ambient
  fragColor += vec4(ka * material.ambient, 0.0);

  for (int i = 0; i < lightCount; i++) {
    Light light = lights[i];

    float distance = length(light.pos - worldSpacePosition);

    float fAtt = 1.0;
    if (light.type == 0 || light.type == 2) { // Point or spot light
        fAtt = calculateAttenuation(light, distance);
    }

    vec3 lightDirection = calculateLightDirection(light, worldSpacePosition);

    float spotFactor = 1.0;
    if (light.type == 2) { // Spot light
        spotFactor = calculateSpotLightFactor(light, lightDirection);
    }

    // Diffuse
    float nl = max(dot(normalize(worldSpaceNormal), lightDirection), 0.0);
    fragColor += vec4(spotFactor * fAtt * kd * nl * material.diffuse * light.color, 0.0);

    // Specular
    vec3 r = reflect(-lightDirection, normalize(worldSpaceNormal));
    vec3 e = normalize(worldSpaceCamera - worldSpacePosition);
    float re = clamp(dot(r, e), 0.0, 1.0);
    if (re == 0.0 && material.shininess <= 0.0) continue;
    float reShininess = pow(re, material.shininess);
    fragColor += spotFactor * fAtt * vec4(ks * reShininess * material.specular * light.color, 0.0);
  }

  fragColor = clamp(fragColor, 0.0, 1.0);
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

type context = {
  cameras : (Camera.Projection.t * Transform.t option) list;
  entities : (Mesh3d.t * Material.t * Transform.t option) list;
  point_lights : (Light.Point.t * Transform.t option) list;
  directional_lights : (Light.Directional.t * Transform.t option) list;
  spot_lights : (Light.Spot.t * Transform.t option) list;
}

let query w =
  let open Ecs in
  let cameras =
    World.query ~filter:(Query.Filter.With Camera.Camera3d.C.id) w
      Query.(Req (module Camera.Projection.C) @ Opt (module Transform.C) @ Nil)
    |> List.map (fun (_, (p, (t, ()))) -> (p, t))
  in

  let entities =
    World.query w ~filter:(Query.Filter.With C.id)
      Query.(
        Req (module Mesh3d.C)
        @ Req (module Material.C)
        @ Opt (module Transform.C)
        @ Nil)
    |> List.map (fun (_, (m3d, (m, (t, ())))) -> (m3d, m, t))
  in
  let parse_lights l =
    List.map (fun (_, (light, (transform, ()))) -> (light, transform)) l
  in

  let point_lights =
    World.query w
      Query.(Req (module Light.Point.C) @ Opt (module Transform.C) @ Nil)
    |> parse_lights
  in
  let directional_lights =
    World.query w
      Query.(Req (module Light.Directional.C) @ Opt (module Transform.C) @ Nil)
    |> parse_lights
  in
  let spot_lights =
    World.query w
      Query.(Req (module Light.Spot.C) @ Opt (module Transform.C) @ Nil)
    |> parse_lights
  in
  { cameras; entities; point_lights; directional_lights; spot_lights }

let load_lights pid point_lights directional_lights spot_lights =
  let light_index = ref 0 in
  let light_loc field = "lights[" ^ string_of_int !light_index ^ "]." ^ field in
  let load_light t color attenuation =
    load_uniform1i t pid (light_loc "type");
    load_uniform3fv color pid (light_loc "color");
    load_uniform3fv attenuation pid (light_loc "attenuation")
  in

  List.iter
    (fun (point, transform) ->
      load_light 0 (Light.Point.color point) (Light.Point.attenuation point);
      (match transform with
      | Some transform ->
          let pos = Transform.translation transform in
          load_uniform3fv pos pid (light_loc "pos")
      | None -> ());
      light_index := !light_index + 1)
    point_lights;

  List.iter
    (fun (directional, transform) ->
      load_light 1
        (Light.Directional.color directional)
        (Light.Directional.attenuation directional);
      (match transform with
      | Some transform ->
          let rot = Transform.rotation transform in
          let dir = Math.Quat.apply3 rot (Math.Vec3.v 0. 0. (-1.)) in
          load_uniform3fv dir pid (light_loc "dir")
      | None -> ());
      light_index := !light_index + 1)
    directional_lights;

  List.iter
    (fun (spot, transform) ->
      load_light 2 (Light.Spot.color spot) (Light.Spot.attenuation spot);
      load_uniform1f (Light.Spot.angle spot) pid (light_loc "angle");
      load_uniform1f (Light.Spot.penumbra spot) pid (light_loc "penumbra");
      (match transform with
      | Some transform ->
          let pos = Transform.translation transform in
          load_uniform3fv pos pid (light_loc "pos");
          let rot = Transform.rotation transform in
          let dir = Math.Quat.apply3 rot (Math.Vec3.v 0. 0. (-1.)) in
          load_uniform3fv dir pid (light_loc "dir")
      | None -> ());
      light_index := !light_index + 1)
    spot_lights;

  load_uniform1i !light_index pid "lightCount"

let render pid
    { cameras; entities; point_lights; directional_lights; spot_lights } =
  (* print number of lights *)
  let render_entity ?(transform = Transform.identity ()) m3d mat =
    let transform = Transform.compute_matrix transform in
    load_matrix4fv transform pid "modelMatrix";

    let normal_matrix =
      Math.Mat3.inv (Math.Mat3.transpose (Math.Mat3.of_m4 transform))
    in
    load_matrix3fv normal_matrix pid "normalMatrix";

    load_uniform3fv (Material.ambient mat) pid "material.ambient";
    load_uniform3fv (Material.diffuse mat) pid "material.diffuse";
    load_uniform3fv (Material.specular mat) pid "material.specular";
    load_uniform1f (Material.shininess mat) pid "material.shininess";

    Mesh3d.draw m3d
  in

  let render_to_camera ?(ctransform = Transform.identity ()) proj =
    let view = Math.Mat4.inv (Transform.compute_matrix ctransform) in
    load_matrix4fv view pid "viewMatrix";
    let projection = Camera.Projection.project proj in
    load_matrix4fv projection pid "projectionMatrix";

    let world_space_camera = Transform.translation ctransform in
    load_uniform3fv world_space_camera pid "worldSpaceCamera";

    load_lights pid point_lights directional_lights spot_lights;

    List.iter
      (fun (m3d, mat, transform) -> render_entity ?transform m3d mat)
      entities
  in
  List.iter (fun (p, ctransform) -> render_to_camera ?ctransform p) cameras
