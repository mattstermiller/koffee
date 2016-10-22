namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions

type MainController(pathing: IPathService, settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- pathing.Root
            model.Nodes <- pathing.GetNodes model.Path

        member this.Dispatcher = function
            | CursorUp -> Sync (fun m -> this.SetCursor (m.Cursor - 1) m)
            | CursorUpHalfPage -> Sync (fun m -> this.SetCursor (m.Cursor - m.HalfPageScroll) m)
            | CursorDown -> Sync (fun m -> this.SetCursor (m.Cursor + 1) m)
            | CursorDownHalfPage -> Sync (fun m -> this.SetCursor (m.Cursor + m.HalfPageScroll) m)
            | CursorToFirst -> Sync (this.SetCursor 0)
            | CursorToLast -> Sync (fun m -> this.SetCursor (m.Nodes.Length - 1) m)
            | OpenPath p -> Sync (this.OpenPath (Path p))
            | OpenSelected -> Sync this.SelectedPath
            | OpenParent -> Sync this.ParentPath
            | OpenExplorer -> Sync this.OpenExplorer
            | Find c -> Sync (this.Find c)
            | FindNext -> Sync this.FindNext
            | Search str -> Sync (this.Search str false)
            | SearchNext -> Sync (this.SearchNext false)
            | SearchPrevious -> Sync (this.SearchNext true)
            | TogglePathFormat -> Sync this.TogglePathFormat
            | OpenSettings -> Sync this.OpenSettings
            | StartInput inputType -> Sync (this.StartInput inputType)

    member this.SetCursor index (model: MainModel) =
        model.Cursor <- index |> max 0 |> min (model.Nodes.Length - 1)

    member this.MoveCursorToNext predicate reverse (model: MainModel) =
        let indexed = model.Nodes |> List.mapi (fun i n -> (i, n))
        let items =
            if reverse then
                Seq.append indexed.[model.Cursor..] indexed.[0..(model.Cursor-1)]
                |> Seq.rev
            else
                Seq.append indexed.[(model.Cursor+1)..] indexed.[0..model.Cursor]
        let firstMatch =
            items
            |> Seq.choose (fun (i, n) -> if predicate n then Some i else None)
            |> Seq.tryHead
        match firstMatch with
        | Some index -> this.SetCursor index model
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
        this.MoveCursorToNext (fun n -> n.Name.[0] = char) false model

    member this.FindNext (model: MainModel) =
        match model.LastFind with
        | Some c -> this.Find c model
        | None -> ()

    member this.Search searchStr reverse (model: MainModel) =
        model.LastSearch <- Some searchStr
        model.Status <- sprintf "Search \"%s\"" searchStr
        this.MoveCursorToNext (fun n -> Regex.IsMatch(n.Name, searchStr, RegexOptions.IgnoreCase)) reverse model

    member this.SearchNext reverse (model: MainModel) =
        match model.LastSearch with
        | Some str -> this.Search str reverse model
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

    member this.OpenSettings (model: MainModel) =
        let settings = settingsFactory()
        settings.StartDialog() |> ignore
