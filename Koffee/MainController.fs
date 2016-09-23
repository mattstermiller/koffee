namespace Koffee

open FSharp.Desktop.UI

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
