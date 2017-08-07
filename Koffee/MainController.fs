namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions
open System.Threading.Tasks
open Utility
open ModelExtensions
open Koffee.ConfigExt

type MainController(fileSys: IFileSystemService,
                    settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>,
                    config: Config,
                    commandLinePath: string option) =
    let runAsync (f: unit -> 'a) = f |> Task.Run |> Async.AwaitTask
    let mutable taskRunning = false

    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            config.Changed.Add (fun _ ->
                model.PathFormat <- config.PathFormat
                model.ShowFullPathInTitle <- config.Window.ShowFullPathInTitle)
            config.Load()
            model.Path <- config.DefaultPath |> Path.Parse |> Option.coalesce Path.Root

            let startupPath =
                commandLinePath |> Option.coalesce (
                    match config.StartupPath with
                    | RestorePrevious -> config.PreviousPath
                    | DefaultPath -> config.DefaultPath)

            this.OpenUserPath (startupPath) model

        member this.Dispatcher = this.LockingDispatcher

    member this.LockingDispatcher evt =
        match this.Dispatcher evt with
        | Sync handler -> Sync (fun m -> if not taskRunning then handler m)
        | Async handler ->
            Async (fun m -> async {
                if not taskRunning then
                    taskRunning <- true
                    do! handler m
                    taskRunning <- false
            })

    member this.Dispatcher evt : EventHandler<MainModel> =
        match evt with
        | CursorUp -> Sync (fun m -> m.SetCursor (m.Cursor - 1))
        | CursorUpHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor - m.HalfPageScroll))
        | CursorDown -> Sync (fun m -> m.SetCursor (m.Cursor + 1))
        | CursorDownHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor + m.HalfPageScroll))
        | CursorToFirst -> Sync (fun m -> m.SetCursor 0)
        | CursorToLast -> Sync (fun m -> m.SetCursor (m.Nodes.Length - 1))
        | OpenPath p -> Sync (this.OpenUserPath p)
        | OpenSelected -> Sync this.OpenSelected
        | OpenParent -> Sync this.OpenParent
        | Back -> Sync this.Back
        | Forward -> Sync this.Forward
        | Refresh -> Sync this.Refresh
        | Undo -> Async this.Undo
        | Redo -> Async this.Redo
        | StartInput inputMode -> Sync (this.StartInput inputMode)
        | ExecuteCommand -> Sync this.ExecuteCommand
        | CommandCharTyped c -> Async (this.CommandCharTyped c)
        | FindNext -> Sync this.FindNext
        | SearchNext -> Sync (this.SearchNext false)
        | SearchPrevious -> Sync (this.SearchNext true)
        | StartMove -> Sync this.StartMove
        | StartCopy -> Sync this.StartCopy
        | Put -> Async (this.Put false)
        | Recycle -> Async this.Recycle
        | PromptDelete -> Sync (this.StartInput (Confirm Delete))
        | SortList field -> Sync (this.SortList field)
        | ToggleHidden -> Sync this.ToggleHidden
        | OpenSettings -> Sync this.OpenSettings
        | OpenExplorer -> Sync this.OpenExplorer
        | OpenCommandLine -> Sync this.OpenCommandLine
        | OpenWithTextEditor -> Sync this.OpenWithTextEditor
        | Exit -> Sync ignore // handled by view

    member this.OpenUserPath pathStr model =
        match Path.Parse pathStr with
        | Some path ->
            match fileSys.GetNode path with
            | Some node when node.Type = File ->
                this.OpenPath path.Parent (SelectName node.Name) model
            | _ ->
                this.OpenPath path KeepSelect model
        | None -> model.Status <- Some <| MainStatus.invalidPath pathStr

    member this.OpenPath path select model =
        try
            let sortField, sortDesc = model.Sort
            let sorter = SortField.SortByTypeThen sortField sortDesc
            let nodes = fileSys.GetNodes path config.ShowHidden |> sorter
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
            model.Status <- None
        with | ex -> model.Status <- Some <| StatusType.fromExn "open path" ex

    member this.OpenSelected model =
        let path = model.SelectedNode.Path
        match model.SelectedNode.Type with
            | Folder | Drive | Error ->
                this.OpenPath path KeepSelect model
            | File ->
                try
                    fileSys.OpenFile path
                    model.Status <- Some <| MainStatus.openFile (path.Format model.PathFormat)
                with | ex ->
                    model.Status <- Some <| MainStatus.couldNotOpenFile path.Name ex.Message

    member this.OpenParent model =
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

    member this.Refresh model =
        this.OpenPath model.Path KeepSelect model

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
            | Some GoToBookmark -> ()
            | Some SetBookmark -> ()
            | Some DeleteBookmark -> ()
            | Some (Confirm _) -> ()

    member this.CommandCharTyped char model = async {
        match model.CommandInputMode with
        | Some Find ->
            this.Find char model
            model.CommandInputMode <- None
        | Some GoToBookmark ->
            model.CommandInputMode <- None
            match config.GetBookmark char with
                | Some path -> this.OpenUserPath path model
                | None -> model.Status <- Some <| MainStatus.noBookmark char
        | Some SetBookmark ->
            model.CommandInputMode <- None
            config.SetBookmark char (model.Path.Format Windows)
            config.Save()
            model.Status <- Some <| MainStatus.setBookmark char model.PathFormatted
        | Some DeleteBookmark ->
            model.CommandInputMode <- None
            match config.GetBookmark char with
                | Some path ->
                    config.RemoveBookmark char
                    config.Save()
                    model.Status <- Some <| MainStatus.deletedBookmark char path
                | None -> model.Status <- Some <| MainStatus.noBookmark char
        | Some (Confirm confirmType) ->
            match char with
            | 'y' -> 
                model.CommandInputMode <- None
                match confirmType with
                    | Overwrite -> do! this.Put true model
                    | Delete -> do! this.Delete model.SelectedNode true model
            | 'n' ->
                model.CommandInputMode <- None
                match confirmType with
                    | Overwrite when not config.ShowHidden && model.Nodes |> Seq.exists (fun n -> n.IsHidden) ->
                        // if we were temporarily showing hidden files, refresh
                        this.Refresh model
                    | _ -> ()
                model.Status <- Some <| MainStatus.cancelled
            | _ ->
                model.CommandText <- ""
        | _ -> ()
    }

    member this.Find char model =
        model.LastFind <- Some char
        model.Status <- Some <| MainStatus.find char
        this.MoveCursorToNext (fun n -> n.Name.[0] = char) false model

    member this.FindNext model =
        match model.LastFind with
        | Some c -> this.Find c model
        | None -> ()

    member this.Search searchStr reverse model =
        let search = if searchStr <> "" then Some searchStr else None

        // if search is different or empty (to clear results), update node flags
        if search.IsNone || not <| MainStatus.isSearchStatus searchStr model.Status then
            let cursor = model.Cursor
            model.Nodes <-
                model.Nodes
                |> Seq.map (fun n ->
                    let isMatch =
                        match search with
                        | Some str -> Regex.IsMatch(n.Name, str, RegexOptions.IgnoreCase)
                        | None -> false
                    if isMatch && not n.IsSearchMatch then { n with IsSearchMatch = true }
                    else if not isMatch && n.IsSearchMatch then { n with IsSearchMatch = false }
                    else n)
                |> Seq.toList
            model.Cursor <- cursor
            let matches = model.Nodes |> Seq.filter (fun n -> n.IsSearchMatch) |> Seq.length
            model.Status <- search |> Option.map (MainStatus.search matches)

        if search.IsSome then
            model.LastSearch <- search

        this.MoveCursorToNext (fun n -> n.IsSearchMatch) reverse model

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
            let action = CreatedItem { Path = model.Path; Name = name; Type = nodeType;
                                       Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }
            model |> MainStatus.setActionExceptionStatus action ex

    member this.Rename node newName model =
        let action = RenamedItem (node, newName)
        try
            let newPath = node.Path.Parent.Join newName
            fileSys.Move node.Path newPath
            this.OpenPath model.Path (SelectName newName) model
            model |> this.PerformedAction action
        with | ex -> model |> MainStatus.setActionExceptionStatus action ex

    member this.StartMove model =
        match model.SelectedNode.Type with
        | File | Folder ->
            model.ItemBuffer <- Some (model.SelectedNode, Move)
            model.Status <- None
        | _ -> ()

    member this.StartCopy model =
        match model.SelectedNode.Type with
        | File | Folder ->
            model.ItemBuffer <- Some (model.SelectedNode, Copy)
            model.Status <- None
        | _ -> ()

    member this.Put overwrite model = async {
        match model.ItemBuffer with
        | Some (node, bufferAction) ->
            let sameFolder = node.Path.Parent = model.Path
            if bufferAction = Move && sameFolder then
                model.Status <- Some MainStatus.cannotMoveToSameFolder
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
                match fileSys.GetNode newPath with
                    | Some existing when not overwrite ->
                        // refresh node list to make sure we can see the existing file
                        // if the existing node is hidden, temporarily override ShowHidden
                        let overrideShowHidden = existing.IsHidden && not config.ShowHidden
                        if overrideShowHidden then config.ShowHidden <- true
                        this.OpenPath model.Path (SelectName existing.Name) model
                        if overrideShowHidden then config.ShowHidden <- false
                        this.StartInput (Confirm Overwrite) model
                    | _ ->
                        let fileSysAction, action =
                            match bufferAction with
                            | Move -> (fileSys.Move, MovedItem (node, newPath))
                            | Copy -> (fileSys.Copy, CopiedItem (node, newPath))
                        try
                            model.Status <- MainStatus.runningAction action model.PathFormat
                            do! runAsync (fun () -> fileSysAction node.Path newPath)
                            this.OpenPath model.Path (SelectName newName) model
                            model.ItemBuffer <- None
                            model |> this.PerformedAction action
                        with | ex -> model |> MainStatus.setActionExceptionStatus action ex
        | None -> ()
    }

    member this.Recycle model = async {
        let node = model.SelectedNode
        model.Status <- Some <| MainStatus.checkingIsRecyclable
        let! isRecyclable = runAsync (fun () -> fileSys.IsRecyclable node.Path)
        if isRecyclable then
            do! this.Delete node false model
        else
            model.Status <- Some <| MainStatus.cannotRecycle node
    }

    member private this.Delete node permanent model = async {
        let action = DeletedItem (node, permanent)
        try
            let fileSysFunc =
                if permanent then fileSys.Delete
                else fileSys.Recycle
            model.Status <- MainStatus.runningAction action model.PathFormat
            do! runAsync (fun () -> fileSysFunc node.Path)
            this.Refresh model
            model |> this.PerformedAction action
        with | ex -> model |> MainStatus.setActionExceptionStatus action ex
    }

    member private this.PerformedAction action model =
        model.UndoStack <- action :: model.UndoStack
        model.RedoStack <- []
        model.Status <- Some <| MainStatus.actionComplete action model.PathFormat

    member this.Undo model = async {
        match model.UndoStack with
        | action :: rest ->
            match action with
                | CreatedItem node ->
                    try
                        if fileSys.IsEmpty node.Path then
                            model.Status <- Some <| MainStatus.undoingCreate node
                            do! runAsync (fun () -> fileSys.Delete node.Path)
                            if model.Path = node.Path.Parent then
                                this.Refresh model
                        else
                            model.Status <- Some <| MainStatus.cannotUndoNonEmptyCreated node
                    with | ex ->
                        let action = DeletedItem (node, true)
                        model |> MainStatus.setActionExceptionStatus action ex
                | RenamedItem (oldNode, curName) ->
                    let parentPath = oldNode.Path.Parent
                    let node = { oldNode with Name = curName; Path = parentPath.Join curName}
                    try
                        fileSys.Move node.Path oldNode.Path
                        this.OpenPath parentPath (SelectName oldNode.Name) model
                    with | ex ->
                        let action = RenamedItem (node, oldNode.Name)
                        model |> MainStatus.setActionExceptionStatus action ex
                | MovedItem (node, newPath) ->
                    if fileSys.Exists newPath then
                        // todo: prompt for overwrite here
                        model.Status <- Some <| ErrorMessage (sprintf "Cannot undo move of %s because an item exists in its previous location" node.Name)
                    else
                        try
                            model.Status <- Some <| MainStatus.undoingMove node
                            do! runAsync (fun () -> fileSys.Move newPath node.Path)
                            this.OpenPath node.Path.Parent (SelectName node.Name) model
                        with | ex ->
                            let action = MovedItem ({ node with Path = newPath }, node.Path)
                            model |> MainStatus.setActionExceptionStatus action ex
                | CopiedItem (node, newPath) ->
                    let mutable isDeletionPermanent = false
                    try
                        let copyModified = fileSys.GetNode newPath |> Option.bind (fun n -> n.Modified)
                        isDeletionPermanent <-
                            match node.Modified, copyModified with
                            | Some orig, Some copy when orig = copy -> true
                            | _ -> false
                        let fileSysFunc =
                            if isDeletionPermanent then fileSys.Delete
                            else fileSys.Recycle
                        model.Status <- Some <| MainStatus.undoingCopy node isDeletionPermanent
                        do! runAsync (fun () -> fileSysFunc newPath)
                        if model.Path = newPath.Parent then
                            this.Refresh model
                    with | ex ->
                        let action = DeletedItem ({ node with Path = newPath }, isDeletionPermanent)
                        model |> MainStatus.setActionExceptionStatus action ex
                | DeletedItem (node, permanent) ->
                    model.Status <- Some <| MainStatus.cannotUndoDelete permanent node
            model.UndoStack <- rest
            if not model.HasErrorStatus then
                model.RedoStack <- action :: model.RedoStack
                model.Status <- Some <| MainStatus.undoAction action model.PathFormat
        | [] -> model.Status <- Some <| MainStatus.noUndoActions
    }

    member this.Redo model = async {
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
                    model.Status <- MainStatus.redoingAction action model.PathFormat
                    do! this.Put false model
                    model.ItemBuffer <- buffer
                | DeletedItem (node, permanent) ->
                    goToPath node.Path
                    model.Status <- MainStatus.redoingAction action model.PathFormat
                    do! this.Delete node permanent model
            model.RedoStack <- rest
            model.Status <- Some <| MainStatus.redoAction action model.PathFormat
        | [] -> model.Status <- Some <| MainStatus.noRedoActions
    }

    member this.SortList field model =
        let desc =
            match model.Sort with
            | f, desc when f = field -> not desc
            | _ -> field = Modified
        model.Sort <- field, desc
        this.Refresh model
        model.Status <- Some <| MainStatus.sort field desc

    member this.OpenExplorer model =
        if model.Path <> Path.Root then
            fileSys.OpenExplorer model.SelectedNode.Path
            model.Status <- Some <| MainStatus.openExplorer model.PathFormatted

    member this.OpenCommandLine model =
        if model.Path <> Path.Root then
            fileSys.OpenCommandLine model.Path
            model.Status <- Some <| MainStatus.openCommandLine model.PathFormatted

    member this.OpenWithTextEditor model =
        match model.SelectedNode.Type with
        | File ->
            try
                fileSys.OpenWith config.TextEditor model.SelectedNode.Path
                model.Status <- Some <| MainStatus.openTextEditor (model.SelectedNode.Path.Format model.PathFormat)
            with | ex ->
                model.Status <- Some <| MainStatus.couldNotOpenTextEditor ex.Message
        | _ -> ()

    member this.ToggleHidden model =
        config.ShowHidden <- not config.ShowHidden
        config.Save()

        this.OpenPath model.Path (SelectName model.SelectedNode.Name) model
        model.Status <- Some <| MainStatus.toggleHidden config.ShowHidden

    member this.OpenSettings model =
        let settings = settingsFactory()
        settings.StartDialog() |> ignore

        model.PathFormat <- config.PathFormat
        model.ShowFullPathInTitle <- config.Window.ShowFullPathInTitle
        this.Refresh model


    member private this.SetCommandSelection cursorPos model =
        match cursorPos with
        | Some pos ->
            let fullName = model.CommandText
            let (name, ext) = Path.SplitName fullName
            model.CommandTextSelection <-
                match pos with
                | Begin -> (0, 0)
                | EndName -> (name.Length, 0)
                | End -> (fullName.Length, 0)
                | ReplaceName -> (0, name.Length)
                | ReplaceAll -> (0, fullName.Length)
        | None -> ()


    static member GetCopyName name i =
        let (nameNoExt, ext) = Path.SplitName name
        let number = if i = 0 then "" else sprintf " %i" (i+1)
        sprintf "%s (copy%s)%s" nameNoExt number ext
