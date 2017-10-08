module Koffee.Program

open System.Windows
open FSharp.Desktop.UI

let makeSettingsMvc config =
    let model = SettingsModel.Create()
    let view = SettingsView(SettingsWindow(), config)
    let controller = SettingsController(config)
    Mvc(model, view, controller)

let makeMainMvc config commandLinePath =
    let model = MainModel.Create()
    let window = MainWindow()
    let view = MainView(window, KeyBinding.Defaults, config)
    let fileSys = FileSystemService()
    let settingsFactory = (fun () -> makeSettingsMvc config)
    let controller = MainController(fileSys, settingsFactory, config, commandLinePath)
    (Mvc(model, view, controller), window)

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let commandLinePath = if args.Length > 0 then Some args.[0] else None

    let config = Config()
    let (mvc, window) = makeMainMvc config commandLinePath

    use eventLoop = mvc.Start()

    let app = Application()
    app.Run window |> ignore
    0
