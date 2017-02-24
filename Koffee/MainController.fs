namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions
open System.Threading.Tasks

type MainController(fileSys: IFileSystemService, settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>) =
    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            this.OpenUserPath (model.Path.Format Windows) model
            model.BackStack <- []

        member this.Dispatcher = this.Dispatcher

    member this.Dispatcher evt : EventHandler<MainModel> =
        match evt with
        | CursorUp -> Sync (fun m -> m.SetCursor (m.Cursor - 1))
        | CursorUpHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor - m.HalfPageScroll))
        | CursorDown -> Sync (fun m -> m.SetCursor (m.Cursor + 1))
        | CursorDownHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor + m.HalfPageScroll))
        | CursorToFirst -> Sync (fun m -> m.SetCursor 0)
        | CursorToLast -> Sync (fun m -> m.SetCursor (m.Nodes.Length - 1))
        | OpenPath p -> Sync (this.OpenUserPath p)
        | OpenSelected -> Sync this.SelectedPath
        | OpenParent -> Sync this.ParentPath
        | Back -> Sync this.Back
        | Forward -> Sync this.Forward
        | Refresh -> Sync (fun m -> this.OpenPath m.Path KeepSelect m)
        | Undo -> Sync this.Undo
        | Redo -> Sync this.Redo
        | StartInput inputMode -> Sync (this.StartInput inputMode)
        | ExecuteCommand -> Sync this.ExecuteCommand
        | CommandCharTyped c -> Sync (this.CommandCharTyped c)
        | FindNext -> Sync this.FindNext
        | SearchNext -> Sync (this.SearchNext false)
        | SearchPrevious -> Sync (this.SearchNext true)
        | StartMove -> Sync this.StartMove
        | StartCopy -> Sync this.StartCopy
        | Put -> Sync (this.Put false)
        | Recycle -> Async this.Recycle
        | PromptDelete -> Sync (this.StartInput (Confirm Delete))
        | TogglePathFormat -> Sync this.TogglePathFormat
        | OpenExplorer -> Sync this.OpenExplorer
        | OpenSettings -> Sync this.OpenSettings
        | Exit -> Sync ignore // handled by view

    member this.OpenUserPath pathStr model =
        match Path.Parse pathStr with
        | Some path ->
            match fileSys.GetNode path with
            | Some node when node.Type = File ->
                this.OpenPath path.Parent (SelectName node.Name) model
            | _ ->
                this.OpenPath path KeepSelect model
        | None -> model.SetErrorStatus (MainController.InvalidPathStatus pathStr)

    member this.OpenPath path select model =
        try
            let nodes = fileSys.GetNodes path
            if path <> model.Path then
                model.BackStack <- (model.Path, model.Cursor) :: model.BackStack
                model.ForwardStack <- []
                model.Path <- path
                model.Cursor <- 0
            let keepCursor = model.Cursor
            model.Nodes <- nodes
            model.SetCursor (
                match select with
                | SelectIndex index -> index
                | SelectName name ->
                    List.tryFindIndex (fun n -> n.Name = name) nodes
                    |> (fun i -> defaultArg i model.Cursor)
                | KeepSelect -> keepCursor)
            model.Status <- ""
        with | ex -> model.SetExceptionStatus ex "open path"

    member this.SelectedPath model =
        let path = model.SelectedNode.Path
        match model.SelectedNode.Type with
            | Folder | Drive | Error ->
                this.OpenPath path KeepSelect model
            | File ->
                fileSys.OpenFile path
                model.Status <- MainController.OpenFileStatus (path.Format model.PathFormat)

    member this.ParentPath model =
        this.OpenPath model.Path.Parent (SelectName model.Path.Name) model

    member this.Back model =
        match model.BackStack with
        | (path, cursor) :: backTail ->
            let newForwardStack = (model.Path, model.Cursor) :: model.ForwardStack
            this.OpenPath path (SelectIndex cursor) model
            model.BackStack <- backTail
            model.ForwardStack <- newForwardStack
        | [] -> ()

    member this.Forward model =
        match model.ForwardStack with
        | (path, cursor) :: forwardTail ->
            this.OpenPath path (SelectIndex cursor) model
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
            | Some (Confirm _) -> ()

    member this.CommandCharTyped char model =
        match model.CommandInputMode with
        | Some Find ->
            this.Find char model
            model.CommandInputMode <- None
        | Some (Confirm confirmType) ->
            model.CommandInputMode <- None
            match char with
                | 'y' -> 
                    match confirmType with
                    | Overwrite -> this.Put true model
                    | Delete -> this.Delete model.SelectedNode true model
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
        items
            |> Seq.choose (fun (i, n) -> if predicate n then Some i else None)
            |> Seq.tryHead
            |> Option.iter (fun index -> model.SetCursor index)

    member private this.Create nodeType name model =
        try
            fileSys.Create nodeType (model.Path.Join name)
            this.OpenPath model.Path (SelectName name) model
            model |> this.PerformedAction (CreatedItem model.SelectedNode)
        with | ex ->
            let action = CreatedItem {Path = model.Path; Name = name; Type = nodeType; Modified = None; Size = None}
            model |> MainController.SetActionExceptionStatus action ex

    member this.Rename node newName model =
        let action = RenamedItem (node, newName)
        try
            let newPath = node.Path.Parent.Join newName
            fileSys.Move node.Path newPath
            this.OpenPath model.Path (SelectName newName) model
            model |> this.PerformedAction action
        with | ex -> model |> MainController.SetActionExceptionStatus action ex

    member this.StartMove model =
        match model.SelectedNode.Type with
        | File | Folder ->
            model.ItemBuffer <- Some (model.SelectedNode, Move)
            model.Status <- ""
        | _ -> ()

    member this.StartCopy model =
        match model.SelectedNode.Type with
        | File | Folder ->
            model.ItemBuffer <- Some (model.SelectedNode, Copy)
            model.Status <- ""
        | _ -> ()

    member this.Put overwrite model =
        match model.ItemBuffer with
        | Some (node, bufferAction) ->
            let sameFolder = node.Path.Parent = model.Path
            if bufferAction = Move && sameFolder then
                model.SetErrorStatus MainController.CannotMoveToSameFolderStatus
            else
                let newName =
                    if bufferAction = Copy && sameFolder then
                        Seq.init 99 (MainController.GetCopyName node.Name)
                        |> Seq.pick
                            (fun name ->
                                let path = model.Path.Join name
                                if not (fileSys.Exists path) then Some name else None)
                    else
                        node.Name
                let newPath = model.Path.Join newName
                if fileSys.Exists newPath && not overwrite then
                    this.StartInput (Confirm Overwrite) model
                else
                    let fileSysAction, action =
                        match bufferAction with
                        | Move -> (fileSys.Move, MovedItem (node, newPath))
                        | Copy -> (fileSys.Copy, CopiedItem (node, newPath))
                    try
                        fileSysAction node.Path newPath
                        this.OpenPath model.Path (SelectName newName) model
                        model.ItemBuffer <- None
                        model |> this.PerformedAction action
                    with | ex -> model |> MainController.SetActionExceptionStatus action ex
        | None -> ()

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
        model.Status <- MainController.ActionStatus action model.PathFormat

    member this.Undo model =
        match model.UndoStack with
        | action :: rest ->
            model.IsErrorStatus <- false
            let refreshIfOnPath (path: Path) =
                if model.Path = path.Parent then
                    this.OpenPath model.Path KeepSelect model
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
                    let parentPath = oldNode.Path.Parent
                    let node = { oldNode with Name = curName; Path = parentPath.Join curName}
                    try
                        fileSys.Move node.Path oldNode.Path
                        this.OpenPath parentPath (SelectName oldNode.Name) model
                    with | ex ->
                        let action = RenamedItem (node, oldNode.Name)
                        model |> MainController.SetActionExceptionStatus action ex
                | MovedItem (node, newPath) ->
                    if fileSys.Exists newPath then
                        // todo: prompt for overwrite here
                        model.SetErrorStatus (sprintf "Cannot undo move %s because an item exists in its previous location" node.Name)
                    else
                        try
                            fileSys.Move newPath node.Path
                            this.OpenPath node.Path.Parent (SelectName node.Name) model
                        with | ex ->
                            let action = MovedItem ({ node with Path = newPath }, node.Path)
                            model |> MainController.SetActionExceptionStatus action ex
                | CopiedItem (node, newPath) ->
                    let mutable delete = false
                    try
                        let copyModified = fileSys.GetNode newPath |> Option.bind (fun n -> n.Modified)
                        delete <-
                            match node.Modified, copyModified with
                            | Some orig, Some copy when orig = copy -> true
                            | _ -> false
                        if delete then
                            fileSys.Delete newPath
                        else
                            fileSys.Recycle newPath
                        refreshIfOnPath newPath
                    with | ex ->
                        let action = DeletedItem ({ node with Path = newPath }, delete)
                        model |> MainController.SetActionExceptionStatus action ex
                | DeletedItem (node, permanent) ->
                    model.SetErrorStatus (MainController.CannotUndoDeleteStatus permanent node)
            model.UndoStack <- rest
            if not model.IsErrorStatus then
                model.RedoStack <- action :: model.RedoStack
                model.Status <- MainController.UndoActionStatus action model.PathFormat
        | [] -> model.Status <- MainController.NoUndoActionsStatus

    member this.Redo model =
        match model.RedoStack with
        | action :: rest ->
            let goToPath (nodePath: Path) =
                let path = nodePath.Parent
                if path <> model.Path then
                    this.OpenPath path KeepSelect model
            match action with
                | CreatedItem node ->
                    goToPath node.Path
                    this.Create node.Type node.Name model
                | RenamedItem (node, newName) ->
                    goToPath node.Path
                    this.Rename node newName model
                | MovedItem (node, newPath)
                | CopiedItem (node, newPath) ->
                    let moveOrCopy =
                        match action with
                        | MovedItem _ -> Move
                        | _ -> Copy
                    goToPath newPath
                    let buffer = model.ItemBuffer
                    model.ItemBuffer <- Some (node, moveOrCopy)
                    this.Put false model
                    model.ItemBuffer <- buffer
                | DeletedItem (node, permanent) ->
                    goToPath node.Path
                    this.Delete node permanent model
            model.RedoStack <- rest
            model.Status <- MainController.RedoActionStatus action model.PathFormat
        | [] -> model.Status <- MainController.NoRedoActionsStatus

    member this.TogglePathFormat model =
        model.PathFormat <-
            match model.PathFormat with
            | Windows -> Unix
            | Unix -> Windows
        model.Status <- MainController.ChangePathFormatStatus model.PathFormat

    member this.OpenExplorer model =
        if model.Path <> Path.Root then
            model.SelectedNode.Path |> fileSys.OpenExplorer
            model.Status <- MainController.OpenExplorerStatus model.PathFormatted

    member this.OpenSettings model =
        let settings = settingsFactory()
        settings.StartDialog() |> ignore


    member private this.SetCommandSelection cursorPos model =
        match cursorPos with
        | Some pos ->
            let fullName = model.CommandText
            let (name, ext) = Path.SplitName fullName
            model.CommandTextSelection <-
                match pos with
                | Begin -> (0, 0)
                | EndName  -> (name.Length, 0)
                | End  -> (fullName.Length, 0)
                | ReplaceName -> (0, name.Length)
                | ReplaceExt ->
                    if ext.Length > 0 then
                        (name.Length + 1, ext.Length - 1)
                    else
                        (name.Length, 0)
        | None -> ()


    static member GetCopyName name i =
        let (nameNoExt, ext) = Path.SplitName name
        let number = if i = 0 then "" else sprintf " %i" (i+1)
        sprintf "%s (copy%s)%s" nameNoExt number ext

    // nav messages
    static member FindStatus char = sprintf "Find %O" char
    static member SearchStatus searchStr = sprintf "Search \"%s\"" searchStr

    // action messages
    static member OpenFileStatus path = sprintf "Opened File: %s" path
    static member OpenExplorerStatus path = sprintf "Opened Windows Explorer to: %s" path
    static member InvalidPathStatus path = sprintf "Path format is invalid: %s" path
    static member ChangePathFormatStatus newFormat = sprintf "Changed Path Format to %O" newFormat
    static member ActionStatus action pathFormat =
        match action with
        | CreatedItem node -> sprintf "Created %s" node.Description
        | RenamedItem (node, newName) -> sprintf "Renamed %s to \"%s\"" node.Description newName
        | MovedItem (node, newPath) -> sprintf "Moved %s to \"%s\"" node.Description (newPath.Format pathFormat)
        | CopiedItem (node, newPath) -> sprintf "Copied %s to \"%s\"" node.Description (newPath.Format pathFormat)
        | DeletedItem (node, false) -> sprintf "Sent %s to Recycle Bin" node.Description
        | DeletedItem (node, true) -> sprintf "Deleted %s" node.Description
    static member CannotMoveToSameFolderStatus = "Cannot move item to same folder it is already in"
    static member CannotRecycleStatus node =
        sprintf "Cannot move %s to the recycle bin because it is too large" node.Description
    static member CancelledStatus = "Cancelled"
    static member SetActionExceptionStatus action ex model =
        let actionName =
            match action with
            | CreatedItem node -> sprintf "create %s" node.Description
            | RenamedItem (node, newName) -> sprintf "rename %s" node.Description
            | MovedItem (node, newPath) -> sprintf "move %s to \"%s\"" node.Description (newPath.Format model.PathFormat)
            | CopiedItem (node, newPath) -> sprintf "copy %s to \"%s\"" node.Description (newPath.Format model.PathFormat)
            | DeletedItem (node, false) -> sprintf "recycle %s" node.Description
            | DeletedItem (node, true) -> sprintf "delete %s" node.Description
        model.SetExceptionStatus ex actionName

    // undo/redo messages
    static member UndoActionStatus action pathFormat =
        MainController.ActionStatus action pathFormat |> sprintf "Action undone: %s"
    static member RedoActionStatus action pathFormat =
        MainController.ActionStatus action pathFormat |> sprintf "Action redone: %s"
    static member NoUndoActionsStatus = "No more actions to undo"
    static member NoRedoActionsStatus = "No more actions to redo"
    static member CannotUndoNonEmptyCreatedStatus node =
        sprintf "Cannot undo creation of %s because it is no longer empty" node.Description
    static member CannotUndoDeleteStatus permanent node =
        if permanent then
            sprintf "Cannot undo deletion of %s" node.Description
        else
            sprintf "Cannot undo recycling of %s. Please open the Recycle Bin in Windows Explorer to restore this item" node.Description
