module Koffee.Program

open System.Windows
open VinylUI.Wpf
open ProgramOptions

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
    try run ()
    with e -> logCrash e
#endif
    0
