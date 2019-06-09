module Koffee.Program

open System.Windows
open VinylUI.Wpf
open ProgramOptions

let logError isCrash (e: exn) =
    let typ = if isCrash then "crash" else "error"
    let timestamp = System.DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")
    let logFile = Path.KoffeeData.Join(sprintf "%s_%s.log" typ timestamp).Format Windows
    System.IO.File.WriteAllText(logFile, string e)
    let msg =
        sprintf "Sorry! An unexpected error %s:\n\n"
                (if isCrash then "caused Koffee to crash" else "occurred in Koffee") +
        sprintf "%s\n\n" e.Message +
        sprintf "This error has been logged to: \n%s\n\n" logFile +
        "Please report this as an issue on Koffee's GitHub project.\n\n" +
        "https://github.com/mattstermiller/koffee/issues"
    MessageBox.Show(msg, sprintf "Koffee %s!" typ, MessageBoxButton.OK, MessageBoxImage.Error) |> ignore

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let options = parseArgs (Array.toList args)
    let config = Config()
    let fileSys = FileSystem(config)
    let os = OperatingSystem()
    let openSettings () = Settings.View().ShowDialog(Settings.start config) |> ignore
    let window = MainWindow()
    let closeWindow () = window.Dispatcher.Invoke(window.Close)
    let app = Application()
    let run () =
        config.Load()
        app.Run(window, MainLogic.start fileSys fileSys os window.GetScreenWorkingArea config KeyBinding.defaults
                                        openSettings closeWindow options)
        |> ignore
#if DEBUG
    run ()
#else
    VinylUI.Framework.setErrorHandler (logError false)
    try run ()
    with e -> logError true e
#endif
    0
