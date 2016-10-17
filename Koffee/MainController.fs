namespace Koffee

open FSharp.Desktop.UI

type MainController(pathing: IPathService) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- pathing.Root
            model.Nodes <- pathing.GetNodes model.Path

        member this.Dispatcher = function
            | NavUp -> Sync (fun m -> this.NavTo (m.Cursor - 1) m)
            | NavUpHalfPage -> Sync (fun m -> this.NavTo (m.Cursor - m.HalfPageScroll) m)
            | NavDown -> Sync (fun m -> this.NavTo (m.Cursor + 1) m)
            | NavDownHalfPage -> Sync (fun m -> this.NavTo (m.Cursor + m.HalfPageScroll) m)
            | NavToFirst -> Sync (this.NavTo 0)
            | NavToLast -> Sync (fun m -> this.NavTo (m.Nodes.Length - 1) m)
            | PathChanged -> Sync this.OpenPath
            | OpenSelected -> Sync this.SelectedPath
            | OpenParent -> Sync this.ParentPath
            | OpenExplorer -> Sync this.OpenExplorer
            | Find c -> Sync (this.Find c)
            | RepeatFind -> Sync this.RepeatFind
            | StartInput inputType -> Sync (this.StartInput inputType)
            | TogglePathFormat -> Sync this.TogglePathFormat

    member this.NavTo index (model: MainModel) =
        model.Cursor <- index |> max 0 |> min (model.Nodes.Length - 1)

    member this.OpenPath (model: MainModel) =
        let normalized = pathing.Normalize model.Path
        if model.Path <> normalized then
            model.Path <- normalized
        else
            model.Nodes <- pathing.GetNodes model.Path
            model.Cursor <- 0
            model.Status <- ""

    member this.SelectedPath (model: MainModel) =
        let path = model.SelectedNode.Path
        match model.SelectedNode.Type with
        | Folder | Drive | Error ->
            model.Path <- path
        | File ->
            pathing.OpenFile path
            model.Status <- "Opened File: " + path.Value

    member this.ParentPath (model: MainModel) =
        model.Path <- pathing.Parent model.Path

    member this.Find char (model: MainModel) =
        model.LastFind <- Some char
        model.Status <- "Find: " + char.ToString()
        let spliceAt = model.Cursor + 1
        let firstMatch =
            Seq.append model.Nodes.[spliceAt..] model.Nodes.[0..(spliceAt-1)]
            |> Seq.tryFindIndex (fun n -> n.Name.[0] = char)
            |> Option.map
                (fun seqIndex ->
                    match seqIndex + spliceAt with
                    | i when i >= model.Nodes.Length -> i - model.Nodes.Length
                    | i -> i)
        match firstMatch with
        | Some index -> this.NavTo index model
        | None -> ()

    member this.RepeatFind (model: MainModel) =
        match model.LastFind with
        | Some c -> this.Find c model
        | None -> ()

    member this.OpenExplorer (model: MainModel) =
        if model.Path <> pathing.Root then
            model.SelectedNode.Path |> pathing.OpenExplorer
            model.Status <- "Opened Windows Explorer to: " + model.Path.Value

    member this.TogglePathFormat (model: MainModel) =
        let newFormat =
            match pathing.Format with
            | Windows -> Unix
            | Unix -> Windows
        pathing.Format <- newFormat
        model.Path <- pathing.Normalize model.Path
        model.Status <- "Changed Path Format to " + newFormat.ToString()

    member this.StartInput inputType (model: MainModel) =
        match inputType with
        | FindInput -> model.Status <- "Find: "
