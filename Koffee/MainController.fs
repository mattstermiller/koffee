namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions

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
            | OpenPath p -> Sync (this.OpenPath (Path p))
            | OpenSelected -> Sync this.SelectedPath
            | OpenParent -> Sync this.ParentPath
            | OpenExplorer -> Sync this.OpenExplorer
            | Find c -> Sync (this.Find c)
            | FindNext -> Sync this.FindNext
            | Search str -> Sync (this.Search str)
            | SearchNext -> Sync this.SearchNext
            | TogglePathFormat -> Sync this.TogglePathFormat
            | StartInput inputType -> Sync (this.StartInput inputType)

    member this.NavTo index (model: MainModel) =
        model.Cursor <- index |> max 0 |> min (model.Nodes.Length - 1)

    member this.NavToNextWhere predicate (model: MainModel) =
        let indexed = model.Nodes |> List.mapi (fun i n -> (i, n))
        let firstMatch =
            Seq.append indexed.[(model.Cursor+1)..] indexed.[0..model.Cursor]
            |> Seq.choose (fun (i, n) -> if predicate n then Some i else None)
            |> Seq.tryHead
        match firstMatch with
        | Some index -> this.NavTo index model
        | None -> ()

    member this.OpenPath path (model: MainModel) =
        model.Path <- pathing.Normalize path
        model.Nodes <- pathing.GetNodes model.Path
        model.Cursor <- 0
        model.Status <- ""

    member this.SelectedPath (model: MainModel) =
        let path = model.SelectedNode.Path
        match model.SelectedNode.Type with
        | Folder | Drive | Error ->
            this.OpenPath path model
        | File ->
            pathing.OpenFile path
            model.Status <- "Opened File: " + path.Value

    member this.ParentPath (model: MainModel) =
        this.OpenPath (pathing.Parent model.Path) model

    member this.Find char (model: MainModel) =
        model.LastFind <- Some char
        model.Status <- "Find: " + char.ToString()
        this.NavToNextWhere (fun n -> n.Name.[0] = char) model

    member this.FindNext (model: MainModel) =
        match model.LastFind with
        | Some c -> this.Find c model
        | None -> ()

    member this.Search searchStr (model: MainModel) =
        model.LastSearch <- Some searchStr
        model.Status <- sprintf "Search \"%s\"" searchStr
        this.NavToNextWhere (fun n -> Regex.IsMatch(n.Name, searchStr, RegexOptions.IgnoreCase)) model

    member this.SearchNext (model: MainModel) =
        match model.LastSearch with
        | Some str -> this.Search str model
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
        let cursor = model.Cursor
        this.OpenPath model.Path model
        model.Cursor <- cursor
        model.Status <- "Changed Path Format to " + newFormat.ToString()

    member this.StartInput inputType (model: MainModel) =
        match inputType with
        | FindInput -> model.Status <- "Find: "
        | SearchInput -> ()
