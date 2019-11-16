module Koffee.Program

open System.IO
open System.Windows
open VinylUI.Wpf
open ProgramOptions
open Acadian.FSharp
open Koffee

/// if json config file does not exist, try to load and convert old yaml file
let loadOldConfig () =
    if not <| File.Exists(ConfigFile.FilePath) then
        let getPathType (path: Path) =
            let wpath = path.Format Windows
            try
                if FileInfo(wpath).Exists then Some File
                else if DirectoryInfo(wpath).Exists then Some Folder
                else None
            with _ -> None
        ConfigYaml.LoadAndConvert getPathType
    else None

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
    let defaultConfig, defaultHistory = loadOldConfig () |? (Config.Default, History.Default)
    use config = new ConfigFile(defaultConfig)
    use history = new HistoryFile(defaultHistory)
    let fileSys = FileSystem()
    let os = OperatingSystem()
    let openSettings config = Settings.View().ShowDialog(Settings.start config).Config
    let window = MainWindow()
    let closeWindow () = window.Dispatcher.Invoke(window.Close)
    let start = MainLogic.start fileSys fileSys os window.GetScreenWorkingArea config history KeyBinding.defaults
                                openSettings closeWindow options
    Application().Run(window, start) |> ignore

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
