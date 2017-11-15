module Koffee.Program

open System.Windows
open FSharp.Desktop.UI
open UIHelpers
open ProgramOptions

let makeSettingsMvc config =
    let model = SettingsModel.Create()
    let view = SettingsView(SettingsWindow(), config)
    let controller = SettingsController(config)
    Mvc(model, view, controller)

let makeMainMvc config options =
    let model = MainModel.Create()
    let window = MainWindow()
    let view = MainView(window, KeyBinding.Defaults, config, options)
    let fileSys = FileSystemService(config)
    let settingsFactory () = makeSettingsMvc config
    let controller = MainController(fileSys, settingsFactory, window.GetScreenWorkingArea, config, options)
    (Mvc(model, view, controller), window)

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let options = parseArgs (Array.toList args)

    let config = Config()
    let (mvc, window) = makeMainMvc config options

    use eventLoop = mvc.Start()

    let app = Application()
    app.Run window |> ignore
    0
