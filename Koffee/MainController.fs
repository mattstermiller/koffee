namespace Koffee

open FSharp.Desktop.UI

type IPathService =
    abstract root: Path with get
    abstract parent: Path -> Path
    abstract nodes: Path -> Node list

type MainController(path: IPathService) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- path.root
            model.Nodes <- path.nodes model.Path
            model.Cursor <- 0

        member this.Dispatcher = function
            | NavUp -> Sync (this.Nav -1)
            | NavDown -> Sync (this.Nav 1)
            | PathChanged -> Sync this.OpenPath
            | OpenSelected -> Sync this.SelectedPath
            | OpenParent -> Sync this.ParentPath

    member this.Nav offset (model: MainModel) =
        model.Cursor <- model.Cursor + offset

    member this.OpenPath (model: MainModel) =
        model.Nodes <- path.nodes model.Path
        model.Cursor <- 0

    member this.SelectedPath (model: MainModel) =
        model.Path <- model.SelectedNode.Path

    member this.ParentPath (model: MainModel) =
        model.Path <- path.parent model.Path
