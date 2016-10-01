namespace Koffee

open FSharp.Desktop.UI

type IPathService =
    abstract Root: Path with get
    abstract Parent: Path -> Path
    abstract GetNodes: Path -> Node list
    abstract OpenFile: Path -> unit
    abstract OpenExplorer: Path -> unit

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
            | OpenExplorer -> Sync (fun m -> m.Path |> pathing.OpenExplorer)

    member this.Nav offset (model: MainModel) =
        model.Cursor <- model.Cursor + offset

    member this.OpenPath (model: MainModel) =
        model.Nodes <- pathing.GetNodes model.Path
        model.Cursor <- 0

    member this.SelectedPath (model: MainModel) =
        let path = model.SelectedNode.Path
        match model.SelectedNode.Type with
        | Folder | Drive | Error -> model.Path <- path
        | File -> pathing.OpenFile path

    member this.ParentPath (model: MainModel) =
        model.Path <- pathing.Parent model.Path
