namespace Koffee

open System.Windows.Data
open System.Windows.Input
open FSharp.Desktop.UI

type Node = {Name: string; Type: string; Path: string}

[<AbstractClass>]
type MainModel() =
    inherit Model()

    abstract Path: string with get, set
    abstract Nodes: Node list with get, set
    abstract Cursor: int with get, set

    member this.SelectedNode = this.Nodes.[this.Cursor]

type MainEvents =
    | NavUp
    | NavDown
    | PathChanged
    | OpenSelected
    | OpenParent

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    override this.SetBindings (model: MainModel) =
        Binding.OfExpression <@
            window.PathTextBox.Text <- model.Path
            window.NodeList.ItemsSource <- model.Nodes
            window.NodeList.SelectedIndex <- model.Cursor
        @>

    override this.EventStreams = [
        window.PathTextBox.TextChanged |> Observable.map (fun _ -> PathChanged)
        window.KeyDown
            |> Observable.map this.KeyToEvent
            |> Observable.filter (fun e -> e.IsSome)
            |> Observable.map (fun e -> e.Value)
    ]

    member this.KeyToEvent evt : MainEvents option =
        if Keyboard.Modifiers = ModifierKeys.None then
            evt.Handled <- true
            match evt.Key with
            | System.Windows.Input.Key.J -> Some NavDown
            | System.Windows.Input.Key.K -> Some NavUp
            | System.Windows.Input.Key.H -> Some OpenParent
            | System.Windows.Input.Key.L -> Some OpenSelected
            | _ -> evt.Handled <- false; None
        else None

open System.IO

type MainController(loadNodes: string -> Node list) =
    let noop (m: MainModel) = ()

    let nav offset (model: MainModel) =
        model.Cursor <- model.Cursor + offset

    let openPath (model: MainModel) =
        model.Nodes <- loadNodes model.Path; model.Cursor <- 0

    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- @"C:\"
            model.Nodes <- loadNodes model.Path
            model.Cursor <- 0

        member this.Dispatcher = function
            | NavUp -> Sync (nav -1)
            | NavDown -> Sync (nav 1)
            | PathChanged -> Sync openPath
            | OpenSelected -> Sync (fun model -> model.Path <- model.SelectedNode.Path)
            | OpenParent -> Sync (fun model -> model.Path <- Path.GetDirectoryName model.Path)
