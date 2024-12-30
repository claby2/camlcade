open Tgl4
open Util
module Normal = Normal

type program =
  | Staged of { frag : string; vert : string }
  | Initialized of { pid : int }
  | Destroyed

type t = program ref

let create ~frag ~vert = ref (Staged { frag; vert })

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
  | Staged { frag; vert } ->
      compile vert Gl.vertex_shader >>= fun vert ->
      compile frag Gl.fragment_shader >>= fun frag ->
      let pid = Gl.create_program () in
      Gl.attach_shader pid vert;
      Gl.attach_shader pid frag;
      Gl.link_program pid;
      Gl.delete_shader vert;
      Gl.delete_shader frag;
      Ok pid >>= fun pid -> s := Initialized { pid }
  | Initialized _ -> ()
  | Destroyed -> failwith "attempt to initialize destroyed shader"

let with_shader s f =
  match !s with
  | Initialized { pid } ->
      Gl.use_program pid;
      f pid;
      Gl.use_program 0
  | _ -> failwith "attempt to use uninitialized shader"

let destroy s =
  match !s with
  | Initialized { pid } ->
      Gl.delete_program pid;
      s := Destroyed
  | _ -> ()

(* === Custom shaders === *)
let normal = create ~frag:Normal.frag ~vert:Normal.vert

let shade_normal =
  let shade pid entities projection camera_transform =
    List.iter
      (fun (m, t) ->
        Normal.render pid projection m ?transform:t ?camera_transform)
      entities
  in
  Ecs.System.make Normal.query
    (Ecs.System.Query
       (fun (cameras, entities) ->
         with_shader normal (fun pid ->
             List.iter (fun (p, t) -> shade pid entities p t) cameras)))
