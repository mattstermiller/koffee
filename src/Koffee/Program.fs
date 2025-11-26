module Koffee.Program

open System.IO
open System.Windows
open VinylUI.Wpf
open ProgramOptions
open Acadian.FSharp
open Koffee
open UIHelpers

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
            fs, getScreenBounds, progress, subDirResults, configFile, historyFile, options
        )

    Application().Run(window, controller.Start) |> ignore

[<EntryPoint>]
[<System.STAThread>]
let main args =
#if DEBUG
    run args
#else
    VinylUI.Framework.setErrorHandler (Persistence.logAndShowError false)
    try run args
    with e -> Persistence.logAndShowError true e
#endif
    0
