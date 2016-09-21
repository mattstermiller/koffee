module Koffee.Program

open System.Windows
open System.IO
open FSharp.Desktop.UI

let loadNodes path =
    let pathToNode nodeType path =
        {Name = Path.GetFileName path; Path = path; Type = nodeType}
    Seq.append
        (Directory.EnumerateDirectories path |> Seq.map (pathToNode "Folder"))
        (Directory.EnumerateFiles path |> Seq.map (pathToNode "File"))
    |> Seq.toList

[<System.STAThread>]
do
    let model = MainModel.Create()
    let window = MainWindow()
    let view = MainView(window)
    let controller = MainController(loadNodes)
    let mvc = Mvc(model, view, controller)
    use eventLoop = mvc.Start()

    let app = Application()
    app.Run window |> ignore
