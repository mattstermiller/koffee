namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions

type MainController(pathing: IPathService, settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- pathing.Root
            model.Nodes <- pathing.GetNodes model.Path
            model.BackStack <- []
            model.ForwardStack <- []

        member this.Dispatcher = function
            | CursorUp -> Sync (fun m -> this.SetCursor (m.Cursor - 1) m)
            | CursorUpHalfPage -> Sync (fun m -> this.SetCursor (m.Cursor - m.HalfPageScroll) m)
            | CursorDown -> Sync (fun m -> this.SetCursor (m.Cursor + 1) m)
            | CursorDownHalfPage -> Sync (fun m -> this.SetCursor (m.Cursor + m.HalfPageScroll) m)
            | CursorToFirst -> Sync (this.SetCursor 0)
            | CursorToLast -> Sync (fun m -> this.SetCursor (m.Nodes.Length - 1) m)
            | OpenPath p -> Sync (this.OpenPath (Path p) 0)
            | OpenSelected -> Sync this.SelectedPath
            | OpenParent -> Sync this.ParentPath
            | Back -> Sync this.Back
            | Forward -> Sync this.Forward
            | StartInput inputMode -> Sync (this.StartInput inputMode)
            | ExecuteCommand -> Sync this.ExecuteCommand
            | CommandCharTyped c -> Sync (this.CommandCharTyped c)
            | FindNext -> Sync this.FindNext
            | SearchNext -> Sync (this.SearchNext false)
            | SearchPrevious -> Sync (this.SearchNext true)
            | TogglePathFormat -> Sync this.TogglePathFormat
            | OpenSettings -> Sync this.OpenSettings
            | OpenExplorer -> Sync this.OpenExplorer

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

    member this.OpenPath path cursor (model: MainModel) =
        model.BackStack <- (model.Path, model.Cursor) :: model.BackStack
        model.ForwardStack <- []
        model.Path <- pathing.Normalize path
        model.Nodes <- pathing.GetNodes model.Path
        model.Cursor <- cursor
        model.Status <- ""

    member this.SelectedPath (model: MainModel) =
        let path = model.SelectedNode.Path
        match model.SelectedNode.Type with
        | Folder | Drive | Error ->
            this.OpenPath path 0 model
        | File ->
            pathing.OpenFile path
            model.Status <- "Opened File: " + path.Value

    member this.ParentPath (model: MainModel) =
        let path = model.Path
        this.OpenPath (pathing.Parent model.Path) 0 model
        let index = model.Nodes |> Seq.tryFindIndex (fun n -> n.Path = path)
        match index with
            | Some i -> model.Cursor <- i
            | None -> ()

    member this.Back (model: MainModel) =
        match model.BackStack with
        | (path, cursor) :: backTail ->
            let newForwardStack = (model.Path, model.Cursor) :: model.ForwardStack
            this.OpenPath path cursor model
            model.BackStack <- backTail
            model.ForwardStack <- newForwardStack
        | [] -> ()

    member this.Forward (model: MainModel) =
        match model.ForwardStack with
        | (path, cursor) :: forwardTail ->
            this.OpenPath path cursor model
            model.ForwardStack <- forwardTail
        | [] -> ()

    member this.StartInput inputMode (model: MainModel) =
        model.CommandText <- ""
        model.CommandInputMode <- Some inputMode

    member this.ExecuteCommand (model: MainModel) =
        match model.CommandInputMode with
        | Some SearchInput -> this.Search model.CommandText false model
        | Some FindInput -> ()
        | None -> ()

    member this.CommandCharTyped char (model: MainModel) =
        if model.CommandInputMode = Some FindInput then
            this.Find char model

    member this.Find char (model: MainModel) =
        model.LastFind <- Some char
        model.Status <- "Find " + char.ToString()
        model.CommandInputMode <- None
        this.MoveCursorToNext (fun n -> n.Name.[0] = char) false model

    member this.FindNext (model: MainModel) =
        match model.LastFind with
        | Some c -> this.Find c model
        | None -> ()

    member this.Search searchStr reverse (model: MainModel) =
        model.LastSearch <- Some searchStr
        model.Status <- sprintf "Search \"%s\"" searchStr
        model.CommandInputMode <- None
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
        this.OpenPath model.Path model.Cursor model
        model.Status <- "Changed Path Format to " + newFormat.ToString()

    member this.OpenSettings (model: MainModel) =
        let settings = settingsFactory()
        settings.StartDialog() |> ignore
