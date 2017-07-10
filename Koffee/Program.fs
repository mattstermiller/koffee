module Koffee.Program

open System.Windows
open FSharp.Desktop.UI

let makeSettingsMvc config =
    let window = SettingsWindow()
    let model = SettingsModel.Create()
    let view = SettingsView(window, config)
    let controller = SettingsController(config)
    Mvc(model, view, controller)

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let commandLinePath = if args.Length > 0 then Some args.[0] else None

    let config = Config()
    let model = MainModel.Create()
    let window = MainWindow()
    let view = MainView(window, KeyBinding.Defaults, config)
    let fileSys = FileSystemService()
    let settingsFactory = (fun () -> makeSettingsMvc config)
    let controller = MainController(fileSys, settingsFactory, config, commandLinePath)
    let mvc = Mvc(model, view, controller)

    use eventLoop = mvc.Start()

    let app = Application()
    app.Run window |> ignore
    0
