module Koffee.Program

open System.IO
open System.Windows
open VinylUI.Wpf
open ProgramOptions
open Acadian.FSharp
open Koffee

let logError isCrash (e: exn) =
    let typ = if isCrash then "crash" else "error"
    let timestamp = System.DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")
    let logFile = Path.KoffeeData.Join(sprintf "%s_%s.log" typ timestamp).Format Windows
    let logWritten =
        try
            File.WriteAllText(logFile, string e)
            sprintf "This error has been logged to: \n%s\n\n" logFile
        with _ -> ""
    let msg =
        sprintf "Sorry! An unexpected error %s:\n\n"
                (if isCrash then "caused Koffee to crash" else "occurred in Koffee") +
        sprintf "%s\n\n" e.Message +
        logWritten +
        "Please report this as an issue on Koffee's GitHub project:\n" +
        "https://github.com/mattstermiller/koffee/issues"
    MessageBox.Show(msg, sprintf "Koffee %s!" typ, MessageBoxButton.OK, MessageBoxImage.Error) |> ignore

let run args =
    let options = parseArgs (Array.toList args)
    let dir = Path.KoffeeData.Format Windows
    if not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore
    use config = new ConfigFile(Config.Default)
    use history = new HistoryFile(History.Default)
    let fileSys = FileSystem()
    let os = OperatingSystem()
    let openSettings config = Settings.View().ShowDialog(Settings.start config).Config
    let window = KoffeeUI.MainWindow()
    let closeWindow () = window.Dispatcher.Invoke(window.Close)
    let controller = MainLogic.Controller(fileSys, os, window.GetScreenWorkingArea, config, history, KeyBinding.defaults,
                                          openSettings, closeWindow, options)
    Application().Run(window, controller.Start) |> ignore

[<EntryPoint>]
[<System.STAThread>]
let main args =
#if DEBUG
    run args
#else
    VinylUI.Framework.setErrorHandler (logError false)
    try run args
    with e -> logError true e
#endif
    0
