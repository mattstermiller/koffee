module Koffee.Program

open System.Windows
open System.IO
open FSharp.Desktop.UI

let makeSettingsMvc () =
    let window = SettingsWindow()
    let model = SettingsModel.Create()
    let view = SettingsView(window)
    let controller = SettingsController()
    Mvc(model, view, controller)

[<System.STAThread>]
do
    let window = MainWindow()
    let model = MainModel.Create()
    let view = MainView(window, KeyBinding.Defaults)
    let controller = MainController(FileSystemService(), makeSettingsMvc)
    let mvc = Mvc(model, view, controller)
    use eventLoop = mvc.Start()

    let app = Application()
    app.Run window |> ignore
