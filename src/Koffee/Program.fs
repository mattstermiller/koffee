module Koffee.Program

open System.IO
open System.Windows
open VinylUI.Wpf
open ProgramOptions
open Acadian.FSharp
open Koffee
open UIHelpers

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
    let dir = Path.KoffeeData.Format Windows
    if not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore

    let options = parseArgs (Array.toList args)
    use configFile = new ConfigFile(Config.Default)
    use historyFile = new HistoryFile(History.Default)
    let fs = FileSystem()
    let os = OperatingSystem()
    let window = KoffeeUI.MainWindow()
    let getScreenBounds = window.GetScreenWorkingArea
    let progress = Progress()
    let subDirResults = Event<_>()
    let closeWindow () = window.Dispatcher.Invoke(window.Close)

    let controller =
        MainController.Controller(
            CursorCommands.Handler(DataGridScroller(window.ItemGrid)),
            NavigationCommands.Handler(fs, os, progress, subDirResults, closeWindow),
            ItemActionCommands.Handler(fs, os, progress),
            WindowCommands.Handler(fs, os, getScreenBounds, Settings.showDialog window, closeWindow),
            fs, getScreenBounds, progress, subDirResults, KeyBinding.defaults, configFile, historyFile, options
        )

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
