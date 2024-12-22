open Tsdl
open Tgl4

let ( >>= ) x f =
  match x with Ok v -> f v | Error (`Msg msg) -> raise (Failure msg)

module T = struct
  type t = {
    mutable win : Sdl.window option;
    mutable ctx : Sdl.gl_context option;
  }

  let empty () = { win = None; ctx = None }

  let initialize ~gl t =
    let create_window ~gl:(maj, min) =
      let w_atts = Sdl.Window.(opengl + resizable) in
      let w_title = Printf.sprintf "OpenGL %d.%d" maj min in
      let set a v = Sdl.gl_set_attribute a v in
      set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core >>= fun () ->
      set Sdl.Gl.context_major_version maj >>= fun () ->
      set Sdl.Gl.context_minor_version min >>= fun () ->
      set Sdl.Gl.doublebuffer 1 >>= fun () ->
      Sdl.create_window ~w:640 ~h:480 w_title w_atts >>= fun win ->
      Sdl.gl_create_context win >>= fun ctx ->
      Sdl.gl_make_current win ctx >>= fun () -> Ok (win, ctx)
    in

    Sdl.init Sdl.Init.video >>= fun () ->
    create_window ~gl >>= fun (win, ctx) ->
    t.win <- Some win;
    t.ctx <- Some ctx;
    Ok () >>= ignore

  let draw t =
    match t.win with
    | Some win ->
        Gl.clear_color 0. 0. 0. 1.;
        Gl.clear Gl.color_buffer_bit;
        Sdl.gl_swap_window win
    | None -> ()

  let destroy t =
    t.win |> Option.iter Sdl.destroy_window;
    t.ctx |> Option.iter Sdl.gl_delete_context
end

module C = Ecs.Component.Make (T)
