namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions

type MainController(fileSys: IFileSystemService, settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- fileSys.Root
            model.Nodes <- fileSys.GetNodes model.Path
            model.BackStack <- []
            model.ForwardStack <- []

        member this.Dispatcher = this.Dispatcher

    member this.Dispatcher evt : EventHandler<MainModel> =
        match evt with
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
        | Refresh -> Sync (fun m -> this.OpenPath m.Path m.Cursor m)
        | StartInput inputMode -> Sync (this.StartInput inputMode)
        | ExecuteCommand -> Sync this.ExecuteCommand
        | CommandCharTyped c -> Sync (this.CommandCharTyped c)
        | FindNext -> Sync this.FindNext
        | SearchNext -> Sync (this.SearchNext false)
        | SearchPrevious -> Sync (this.SearchNext true)
        | TogglePathFormat -> Sync this.TogglePathFormat
        | OpenSettings -> Sync this.OpenSettings
        | OpenExplorer -> Sync this.OpenExplorer

    member this.SetCursor index model =
        model.Cursor <- index |> max 0 |> min (model.Nodes.Length - 1)

    member this.OpenPath path cursor model =
        try
            let newPath = fileSys.Normalize path
            let nodes = fileSys.GetNodes newPath
            model.BackStack <- (model.Path, model.Cursor) :: model.BackStack
            model.ForwardStack <- []
            model.Path <- newPath
            model.Nodes <- nodes
            model.Cursor <- cursor
            model.Status <- ""
        with | ex -> model.SetExceptionStatus ex "open path"

    member this.SelectedPath model =
        let path = model.SelectedNode.Path
        match model.SelectedNode.Type with
        | Folder | Drive | Error ->
            this.OpenPath path 0 model
        | File ->
            fileSys.OpenFile path
            model.Status <- "Opened File: " + path.Value

    member this.ParentPath model =
        let oldPath = model.Path
        this.OpenPath (fileSys.Parent model.Path) 0 model
        match model.Nodes |> Seq.tryFindIndex (fun n -> n.Path = oldPath) with
            | Some index -> model.Cursor <- index
            | None -> ()

    member this.Back model =
        match model.BackStack with
        | (path, cursor) :: backTail ->
            let newForwardStack = (model.Path, model.Cursor) :: model.ForwardStack
            this.OpenPath path cursor model
            model.BackStack <- backTail
            model.ForwardStack <- newForwardStack
        | [] -> ()

    member this.Forward model =
        match model.ForwardStack with
        | (path, cursor) :: forwardTail ->
            this.OpenPath path cursor model
            model.ForwardStack <- forwardTail
        | [] -> ()

    member this.StartInput inputMode model =
        if inputMode.AllowedOnNodeType model.SelectedNode.Type then
            model.CommandInputMode <- Some inputMode
            match inputMode with
                | Rename pos ->
                    model.CommandText <- model.SelectedNode.Name
                    this.SetCommandSelection pos model
                | _ ->
                    model.CommandText <- ""

    member this.ExecuteCommand model =
        match model.CommandInputMode with
            | Some Search -> this.Search model.CommandText false model
            | Some CreateFile -> this.CreateFile model
            | Some CreateFolder -> this.CreateFolder model
            | Some (Rename _) -> this.Rename model
            | Some Find -> () // find is executed by typing a char
            | None -> ()
        model.CommandInputMode <- None

    member this.CommandCharTyped char model =
        if model.CommandInputMode = Some Find then
            this.Find char model
            model.CommandInputMode <- None

    member this.Find char model =
        model.LastFind <- Some char
        model.Status <- sprintf "Find %O" char
        this.MoveCursorToNext (fun n -> n.Name.[0] = char) false model

    member this.FindNext model =
        match model.LastFind with
        | Some c -> this.Find c model
        | None -> ()

    member this.Search searchStr reverse model =
        model.LastSearch <- Some searchStr
        model.Status <- sprintf "Search \"%s\"" searchStr
        this.MoveCursorToNext (fun n -> Regex.IsMatch(n.Name, searchStr, RegexOptions.IgnoreCase)) reverse model

    member this.SearchNext reverse model =
        match model.LastSearch with
        | Some str -> this.Search str reverse model
        | None -> ()

    member this.MoveCursorToNext predicate reverse model =
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

    member this.CreateFile model =
        this.CreateItem fileSys.CreateFile "file" model

    member this.CreateFolder model =
        this.CreateItem fileSys.CreateFolder "folder" model

    member private this.CreateItem action itemType model =
        let name = model.CommandText
        try
            action model.Path name
            model.Nodes <- fileSys.GetNodes model.Path
            model.Cursor <- List.findIndex (fun n -> n.Name = name) model.Nodes
            model.Status <- sprintf "Created %s: %s" itemType name
        with | ex -> model.SetExceptionStatus ex (sprintf "create %s %s" itemType name)

    member this.Rename model =
        let oldName = model.SelectedNode.Name
        let newName = model.CommandText
        try
            fileSys.Rename model.SelectedNode newName
            model.Nodes <- fileSys.GetNodes model.Path
            model.Cursor <- List.findIndex (fun n -> n.Name = newName) model.Nodes
            model.Status <- sprintf "Renamed %s to: %s" oldName newName
        with | ex -> model.SetExceptionStatus ex (sprintf "rename %s" oldName)

    member this.OpenExplorer model =
        if model.Path <> fileSys.Root then
            model.SelectedNode.Path |> fileSys.OpenExplorer
            model.Status <- "Opened Windows Explorer to: " + model.Path.Value

    member this.TogglePathFormat model =
        let newFormat =
            match fileSys.Format with
            | Windows -> Unix
            | Unix -> Windows
        fileSys.Format <- newFormat
        this.OpenPath model.Path model.Cursor model
        model.Status <- sprintf "Changed Path Format to %O" newFormat

    member this.OpenSettings model =
        let settings = settingsFactory()
        settings.StartDialog() |> ignore


    member private this.SetCommandSelection cursorPos model =
        let nameLen =
            match model.CommandText.LastIndexOf('.') with
            | -1 -> model.CommandText.Length
            | index -> index
        match cursorPos with
            | Begin -> model.CommandTextSelection <- (0, 0)
            | End  -> model.CommandTextSelection <- (nameLen, 0)
            | Replace -> model.CommandTextSelection <- (0, nameLen)
