open Tsdl
open Tgl4

let ( >>= ) x f =
  match x with Ok v -> f v | Error (`Msg msg) -> raise (Failure msg)

module T = struct
  type data = { win : Sdl.window; ctx : Sdl.gl_context; event : Sdl.event }
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

        let event = Sdl.Event.create () in
        t := Some { win; ctx; event }
    | Some _ -> ()

  let draw t =
    match !t with
    | None -> ()
    | Some { win; _ } ->
        Gl.clear_color 0. 0. 0. 1.;
        Gl.clear Gl.color_buffer_bit;
        Sdl.gl_swap_window win

  let render t =
    match !t with
    | None -> ()
    | Some { event; win; _ } ->
        let key_scancode e =
          Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode)
        in
        let window_event e =
          Sdl.Event.(window_event_enum (get e window_event_id))
        in
        Sdl.wait_event (Some event) >>= fun () ->
        (match Sdl.Event.(enum (get event typ)) with
        | `Quit -> raise Ecs.World.Quit
        | `Key_down when key_scancode event = `Escape -> raise Ecs.World.Quit
        | `Window_event -> (
            match window_event event with
            | `Exposed | `Resized ->
                let w, h = Sdl.get_window_size win in
                Gl.viewport 0 0 w h;
                draw t;
                draw t;
                Ok ()
            | _ -> Ok ())
        | _ -> Ok ())
        >>= ignore

  let destroy t =
    match !t with
    | None -> ()
    | Some { win; ctx; _ } ->
        Sdl.destroy_window win;
        Sdl.gl_delete_context ctx
end

module C = Ecs.Component.Make (T)
