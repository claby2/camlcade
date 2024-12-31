open Tsdl
open Tgl4
open Util

type data = { win : Sdl.window; ctx : Sdl.gl_context }
type t = data option ref

let empty () = ref None

let initialize ~gl t =
  match !t with
  | None ->
      let create_window ~gl:(maj, min) =
        let w_atts = Sdl.Window.(opengl + resizable) in
        let w_title = Printf.sprintf "OpenGL %d.%d" maj min in
        let set a v = Sdl.gl_set_attribute a v in
        set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core
        >>= fun () ->
        set Sdl.Gl.context_major_version maj >>= fun () ->
        set Sdl.Gl.context_minor_version min >>= fun () ->
        set Sdl.Gl.doublebuffer 1 >>= fun () ->
        Sdl.create_window ~w:640 ~h:480 w_title w_atts >>= fun win ->
        Sdl.gl_create_context win >>= fun ctx ->
        Sdl.gl_make_current win ctx >>= fun () -> Ok (win, ctx)
      in

      let win, ctx =
        Sdl.init Sdl.Init.video >>= fun () ->
        create_window ~gl >>= fun (win, ctx) -> (win, ctx)
      in
      let _event = Sdl.Event.create () in

      Gl.enable Gl.depth_test;
      Gl.cull_face Gl.back;

      t := Some { win; ctx }
  | Some _ -> ()

let get_window_size t =
  match !t with
  | None -> invalid_arg "Context.get_window_size"
  | Some { win; _ } ->
      let w, h = Sdl.get_window_size win in
      (w, h)

let render t =
  match !t with None -> () | Some { win; _ } -> Sdl.gl_swap_window win

let destroy t =
  match !t with
  | None -> ()
  | Some { win; ctx; _ } ->
      Sdl.destroy_window win;
      Sdl.gl_delete_context ctx

module Window = Sdl.Window

let set_relative_mouse_mode v = Sdl.set_relative_mouse_mode v >>= fun () -> ()

let set_window_fullscreen t flags =
  match !t with
  | None -> invalid_arg "Context.set_window_fullscreen"
  | Some { win; _ } -> Sdl.set_window_fullscreen win flags >>= fun () -> ()

module C = Ecs.Component.Make (struct
  type inner = t
end)
