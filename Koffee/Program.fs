module Koffee.Program

open System.Windows
open FSharp.Desktop.UI

let makeSettingsMvc () =
    let window = SettingsWindow()
    let model = SettingsModel.Create()
    let view = SettingsView(window)
    let controller = SettingsController()
    Mvc(model, view, controller)

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let model = MainModel.Create()
    let window = MainWindow()
    let view = MainView(window, KeyBinding.Defaults)
    let fileSys = FileSystemService()
    let controller = MainController(fileSys, makeSettingsMvc)
    let mvc = Mvc(model, view, controller)

    if args.Length > 0 then
        model.Path <- Path args.[0]

    use eventLoop = mvc.Start()

    let app = Application()
    app.Run window |> ignore
    0
