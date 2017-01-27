namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions
open System.Threading.Tasks

type MainController(fileSys: IFileSystemService, settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            if model.Path.Value = "" then
                model.Path <- fileSys.Root
            model.Path <- fileSys.Normalize model.Path
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
        | Undo -> Sync this.Undo
        | Redo -> Sync this.Redo
        | StartInput inputMode -> Sync (this.StartInput inputMode)
        | ExecuteCommand -> Sync this.ExecuteCommand
        | CommandCharTyped c -> Sync (this.CommandCharTyped c)
        | FindNext -> Sync this.FindNext
        | SearchNext -> Sync (this.SearchNext false)
        | SearchPrevious -> Sync (this.SearchNext true)
        | Recycle -> Async this.Recycle
        | TogglePathFormat -> Sync this.TogglePathFormat
        | OpenExplorer -> Sync this.OpenExplorer
        | OpenSettings -> Sync this.OpenSettings
        | Exit -> Sync ignore // handled by view

    member this.SetCursor index model =
        model.Cursor <- index |> max 0 |> min (model.Nodes.Length - 1)

    member this.OpenPath path cursor model =
        try
            let newPath = fileSys.Normalize path
            let nodes = fileSys.GetNodes newPath
            if newPath <> model.Path then
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
            | Some CreateFile -> this.Create File model.CommandText model
            | Some CreateFolder -> this.Create Folder model.CommandText model
            | Some (Rename _) -> this.Rename model.SelectedNode model.CommandText model
            | None -> ()
            // below are triggered by typing a char
            | Some Find -> ()
            | Some Delete -> ()

    member this.CommandCharTyped char model =
        match model.CommandInputMode with
        | Some Find ->
            this.Find char model
            model.CommandInputMode <- None
        | Some Delete ->
            model.CommandInputMode <- None
            match char with
                | 'y' -> this.Delete model.SelectedNode true model
                | _ -> model.Status <- MainController.CancelledStatus
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

    member private this.Create nodeType name model =
        try
            fileSys.JoinPath model.Path name
                |> fileSys.Create nodeType
            model.Nodes <- fileSys.GetNodes model.Path
            let index = model.FindNode name
            model.Cursor <- index
            model |> this.PerformedAction (CreatedItem model.Nodes.[index])
        with | ex ->
            let action = CreatedItem {Path = model.Path; Name = name; Type = nodeType; Modified = None; Size = None}
            model |> MainController.SetActionExceptionStatus action ex

    member this.Rename node newName model =
        let action = RenamedItem (node, newName)
        try
            let newPath = fileSys.JoinPath (fileSys.Parent node.Path) newName
            fileSys.Move node.Path newPath
            model.Nodes <- fileSys.GetNodes model.Path
            model.Cursor <- model.FindNode newName
            model |> this.PerformedAction action
        with | ex -> model |> MainController.SetActionExceptionStatus action ex

    member this.Recycle model = async {
        let node = model.SelectedNode
        model.Status <- "Calculating size..."
        let! isRecyclable = (fun () -> fileSys.IsRecyclable node.Path) |> Task.Run |> Async.AwaitTask
        if isRecyclable then
            this.Delete node false model
        else
            model.SetErrorStatus (MainController.CannotRecycleStatus node)
        }

    member private this.Delete node permanent model =
        let action = DeletedItem (node, permanent)
        try
            if permanent then
                fileSys.Delete node.Path
            else
                fileSys.Recycle node.Path
            let cursor = model.Cursor
            model.Nodes <- fileSys.GetNodes model.Path
            model.Cursor <- min cursor (model.Nodes.Length-1)
            model |> this.PerformedAction action
        with | ex -> model |> MainController.SetActionExceptionStatus action ex

    member private this.PerformedAction action model =
        model.UndoStack <- action :: model.UndoStack
        model.RedoStack <- []
        model.Status <- MainController.ActionStatus action

    member this.Undo model =
        match model.UndoStack with
        | action :: rest ->
            model.IsErrorStatus <- false
            let refreshIfOnPath path =
                if model.Path = (fileSys.Parent path) then
                    let cursor = model.Cursor
                    model.Nodes <- fileSys.GetNodes model.Path
                    model.Cursor <- cursor |> min (model.Nodes.Length - 1)
            match action with
                | CreatedItem node ->
                    try
                        if fileSys.IsEmpty node.Path then
                            fileSys.Delete node.Path
                            refreshIfOnPath node.Path
                        else
                            model.SetErrorStatus (MainController.CannotUndoNonEmptyCreatedStatus node)
                    with | ex ->
                        let action = DeletedItem (node, true)
                        model |> MainController.SetActionExceptionStatus action ex
                | RenamedItem (oldNode, curName) ->
                    let parentPath = fileSys.Parent oldNode.Path
                    let node = { oldNode with Name = curName; Path = fileSys.JoinPath parentPath curName}
                    try
                        fileSys.Move node.Path oldNode.Path
                        this.OpenPath parentPath 0 model
                        model.Cursor <- model.FindNode oldNode.Name
                    with | ex ->
                        let action = RenamedItem (node, oldNode.Name)
                        model |> MainController.SetActionExceptionStatus action ex
                | DeletedItem (node, permanent) ->
                    model.SetErrorStatus (MainController.CannotUndoDeleteStatus permanent node)
            model.UndoStack <- rest
            if not model.IsErrorStatus then
                model.RedoStack <- action :: model.RedoStack
                model.Status <- MainController.UndoActionStatus action
        | [] -> model.Status <- MainController.NoUndoActionsStatus

    member this.Redo model =
        match model.RedoStack with
        | action :: rest ->
            let goToPath node =
                let path = fileSys.Parent node.Path
                if path <> model.Path then
                    this.OpenPath path 0 model
            match action with
                | CreatedItem node ->
                    goToPath node
                    this.Create node.Type node.Name model
                | RenamedItem (node, newName) ->
                    goToPath node
                    this.Rename node newName model
                | DeletedItem (node, permanent) ->
                    goToPath node
                    this.Delete node permanent model
            model.RedoStack <- rest
            model.Status <- MainController.RedoActionStatus action
        | [] -> model.Status <- MainController.NoRedoActionsStatus

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
        | DeletedItem (node, false) -> sprintf "Sent %A to Recycle Bin: %s" node.Type node.Name
        | DeletedItem (node, true) -> sprintf "Deleted %A Permanently: %s" node.Type node.Name
    static member CannotRecycleStatus node =
        sprintf "Cannot move \"%s\" to the recycle bin because it is too large" node.Name
    static member CancelledStatus = "Cancelled"
    static member SetActionExceptionStatus action ex model =
        let actionName =
            match action with
            | CreatedItem node -> sprintf "create %A %s" node.Type node.Name
            | RenamedItem (node, newName) -> sprintf "rename %s" node.Name
            | DeletedItem (node, permanent) ->
                let verb = if permanent then "delete" else "recycle"
                sprintf "%s %A %s" verb node.Type node.Name
        model.SetExceptionStatus ex actionName

    // undo/redo messages
    static member UndoActionStatus action = MainController.ActionStatus action |> sprintf "Action undone: %s"
    static member RedoActionStatus action = MainController.ActionStatus action |> sprintf "Action redone: %s"
    static member NoUndoActionsStatus = "No more actions to undo"
    static member NoRedoActionsStatus = "No more actions to redo"
    static member CannotUndoNonEmptyCreatedStatus node =
        sprintf "Cannot undo creation of %A \"%O\" because it is no longer empty" node.Type node.Name
    static member CannotUndoDeleteStatus permanent node =
        if permanent then
            sprintf "Cannot undo permanent deletion of %A \"%O\"" node.Type node.Name
        else
            sprintf "Cannot undo recycling of %A \"%O\". Please open the Recycle Bin in Windows Explorer to restore this item" node.Type node.Name
