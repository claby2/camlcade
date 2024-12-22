open Camlcade

let () =
  let app = App.create () |> App.add_plugin Graphics.plugin in
  App.run app
