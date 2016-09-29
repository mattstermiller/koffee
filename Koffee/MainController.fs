namespace Koffee

open FSharp.Desktop.UI

type IPathService =
    abstract Root: Path with get
    abstract Parent: Path -> Path
    abstract GetNodes: Path -> Node list

type MainController(pathing: IPathService) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- pathing.Root
            model.Nodes <- pathing.GetNodes model.Path
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
        model.Nodes <- pathing.GetNodes model.Path
        model.Cursor <- 0

    member this.SelectedPath (model: MainModel) =
        if model.SelectedNode.Type = Folder then
            model.Path <- model.SelectedNode.Path

    member this.ParentPath (model: MainModel) =
        model.Path <- pathing.Parent model.Path
