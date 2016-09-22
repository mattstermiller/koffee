namespace Koffee

open System.Windows.Data
open System.Windows.Input
open FSharp.Desktop.UI

// MODEL

type Path = Path of string

type Node = {
    Name: string
    Type: string
    Path: Path
}

[<AbstractClass>]
type MainModel() =
    inherit Model()

    abstract Path: Path with get, set
    abstract Nodes: Node list with get, set
    abstract Cursor: int with get, set

    member this.SelectedNode = this.Nodes.[this.Cursor]

type MainEvents =
    | NavUp
    | NavDown
    | PathChanged
    | OpenSelected
    | OpenParent

// VIEW

open System.Windows.Controls

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    let addCol propName =
        let col = DataGridTextColumn()
        col.Binding <- Binding(propName)
        window.NodeList.Columns.Add col

    override this.SetBindings (model: MainModel) =
        Binding.OfExpression <@
            window.NodeList.ItemsSource <- model.Nodes
            window.NodeList.SelectedIndex <- model.Cursor
        @>

        window.NodeList.AutoGenerateColumns <- false
        addCol "Name"
        addCol "Type"

        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.DiscriminatedUnionValue()
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

    override this.EventStreams = [
        window.PathBox.TextChanged |> Observable.map (fun _ -> PathChanged)
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

// CONTROLLER

type IPathService =
    abstract root: Path with get
    abstract parent: Path -> Path
    abstract nodes: Path -> Node list

type MainController(path: IPathService) =
    let nav offset (model: MainModel) =
        model.Cursor <- model.Cursor + offset

    let openPath (model: MainModel) =
        model.Nodes <- path.nodes model.Path
        model.Cursor <- 0

    let selectedPath (model: MainModel) =
        model.Path <- model.SelectedNode.Path

    let parentPath (model: MainModel) =
        model.Path <- path.parent model.Path

    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- path.root
            model.Nodes <- path.nodes model.Path
            model.Cursor <- 0

        member this.Dispatcher = function
            | NavUp -> Sync (nav -1)
            | NavDown -> Sync (nav 1)
            | PathChanged -> Sync openPath
            | OpenSelected -> Sync selectedPath
            | OpenParent -> Sync parentPath
