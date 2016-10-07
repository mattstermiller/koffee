module Koffee.Program

open System.Windows
open System.IO
open FSharp.Desktop.UI

[<System.STAThread>]
do
    let model = MainModel.Create()
    let window = MainWindow()
    let view = MainView(window, KeyBinding.Defaults)
    let controller = MainController(PathService())
    let mvc = Mvc(model, view, controller)
    use eventLoop = mvc.Start()

    let app = Application()
    app.Run window |> ignore
