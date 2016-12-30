namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions

type MainController(fileSys: IFileSystemService, settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            model.Path <- fileSys.Root
            model.Nodes <- fileSys.GetNodes model.Path

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
        | Delete -> Sync this.Delete
        | TogglePathFormat -> Sync this.TogglePathFormat
        | OpenExplorer -> Sync this.OpenExplorer
        | OpenSettings -> Sync this.OpenSettings

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
                model.Status <- MainController.OpenFileStatus path

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
            let text, pos =
                match inputMode with
                    | Rename pos -> model.SelectedNode.Name, (Some pos)
                    | _ -> "", None
            model.CommandInputMode <- Some inputMode
            model.CommandText <- text
            this.SetCommandSelection pos model

    member this.ExecuteCommand model =
        let mode = model.CommandInputMode
        model.CommandInputMode <- None
        match mode with
            | Some Search -> this.Search model.CommandText false model
            | Some CreateFile -> this.Create File model
            | Some CreateFolder -> this.Create Folder model
            | Some (Rename _) -> this.Rename model
            | None -> ()
            // below are triggered by typing a char
            | Some Find -> ()
            | Some DeletePermanently -> ()

    member this.CommandCharTyped char model =
        match model.CommandInputMode with
        | Some Find ->
            this.Find char model
            model.CommandInputMode <- None
        | Some DeletePermanently ->
            model.CommandInputMode <- None
            match char with
                | 'y' -> this.DeleteItem true model
                | _ -> model.Status <- MainController.DeleteCancelledStatus
        | _ -> ()

    member this.Find char model =
        model.LastFind <- Some char
        model.Status <- MainController.FindStatus char
        this.MoveCursorToNext (fun n -> n.Name.[0] = char) false model

    member this.FindNext model =
        match model.LastFind with
        | Some c -> this.Find c model
        | None -> ()

    member this.Search searchStr reverse model =
        model.LastSearch <- Some searchStr
        model.Status <- MainController.SearchStatus searchStr
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

    member private this.Create nodeType model =
        let name = model.CommandText
        try
            fileSys.Create nodeType model.Path name
            model.Nodes <- fileSys.GetNodes model.Path
            let index = model.FindNode name
            let action = CreatedItem model.Nodes.[index]
            model.Cursor <- index
            model.Status <- MainController.ActionStatus action
        with | ex -> model.SetExceptionStatus ex (sprintf "create %A %s" nodeType name)

    member this.Rename model =
        let node = model.SelectedNode
        let oldName = node.Name
        let newName = model.CommandText
        try
            fileSys.Rename node.Type node.Path newName
            model.Nodes <- fileSys.GetNodes model.Path
            model.Cursor <- model.FindNode newName
            let action = RenamedItem (node, newName)
            model.Status <- MainController.ActionStatus action
        with | ex -> model.SetExceptionStatus ex (sprintf "rename %s" oldName)

    member this.Delete model = this.DeleteItem false model

    member private this.DeleteItem permanent model =
        let node = model.SelectedNode
        try
            if permanent then
                fileSys.DeletePermanently node
            else
                fileSys.Delete node
            let cursor = model.Cursor
            model.Nodes <- fileSys.GetNodes model.Path
            model.Cursor <- min cursor (model.Nodes.Length-1)
            let action = DeletedItem (node, permanent)
            model.Status <- MainController.ActionStatus action
        with | ex -> model.SetExceptionStatus ex (sprintf "delete %A %s" node.Type node.Name)

    member this.TogglePathFormat model =
        let newFormat =
            match fileSys.Format with
            | Windows -> Unix
            | Unix -> Windows
        fileSys.Format <- newFormat
        this.OpenPath model.Path model.Cursor model
        model.Status <- MainController.ChangePathFormatStatus newFormat

    member this.OpenExplorer model =
        if model.Path <> fileSys.Root then
            model.SelectedNode.Path |> fileSys.OpenExplorer
            model.Status <- MainController.OpenExplorerStatus model.Path

    member this.OpenSettings model =
        let settings = settingsFactory()
        settings.StartDialog() |> ignore


    member private this.SetCommandSelection cursorPos model =
        match cursorPos with
        | Some pos ->
            let nameLen =
                match model.CommandText.LastIndexOf('.') with
                | -1 -> model.CommandText.Length
                | index -> index
            model.CommandTextSelection <-
                match pos with
                | Begin -> (0, 0)
                | End  -> (nameLen, 0)
                | Replace -> (0, nameLen)
        | None -> ()


    // nav messages
    static member FindStatus char = sprintf "Find %O" char
    static member SearchStatus searchStr = sprintf "Search \"%s\"" searchStr

    // action messages
    static member OpenFileStatus path = sprintf "Opened File: %s" path.Value
    static member OpenExplorerStatus path = sprintf "Opened Windows Explorer to: %s" path.Value
    static member ChangePathFormatStatus newFormat = sprintf "Changed Path Format to %O" newFormat
    static member ActionStatus action =
        match action with
        | CreatedItem node -> sprintf "Created %A: %s" node.Type node.Name
        | RenamedItem (node, newName) -> sprintf "Renamed %s to: %s" node.Name newName
        | DeletedItem (node, false) -> sprintf "Moved %A to Recycle Bin: %s" node.Type node.Name
        | DeletedItem (node, true) -> sprintf "Deleted %A Permanently: %s" node.Type node.Name
    static member DeleteCancelledStatus = "Delete cancelled"
