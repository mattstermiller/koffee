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

let makeMainMvc config options window =
    let model = MainBindModel.Create()
    let view = MainView(window, config, options)
    let fileSys = FileSystem(config)
    let os = OperatingSystem()
    let openSettings () = (makeSettingsMvc config).StartDialog() |> ignore
    let controller = MainController(fileSys, fileSys, os, window.GetScreenWorkingArea, config, KeyBinding.defaults,
                                    openSettings, window.Close, options)
    Mvc(model, view, controller)

let logCrash (e: exn) =
    let timestamp = System.DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")
    let logFile = Path.KoffeeData.Join(sprintf "crash_%s.log" timestamp).Format Windows
    System.IO.File.WriteAllText(logFile, string e)

    let msg =
        "Sorry! An unexpected error caused Koffee to crash:\n\n" +
        sprintf "%s\n\n" e.Message +
        sprintf "This error has been logged to: \n%s\n\n" logFile +
        "Please report this as an issue on Koffee's GitHub project.\n\n" +
        "https://github.com/mattstermiller/koffee/issues"
    MessageBox.Show(msg, "Crash!", MessageBoxButton.OK, MessageBoxImage.Error) |> ignore

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let options = parseArgs (Array.toList args)

    let config = Config()
    let window = MainWindow()
    let mvc = makeMainMvc config options window

    use eventLoop = mvc.Start()

    let app = Application()
    try app.Run window |> ignore
    with e -> logCrash e
    0
