namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions
open System.Threading.Tasks
open Koffee.ConfigExt
open Utility

module MainLogic =
    let initModel (config: Config) getNode openUserPath startupOptions (model: MainModel) =
        config.Changed.Add (fun _ ->
            model.YankRegister <- config.YankRegister
                                  |> Option.bind (fun (path, action) ->
                                      (getNode path, Some action) ||> Option.map2 (fun p a -> p, a))
            model.PathFormat <- config.PathFormat
            model.ShowFullPathInTitle <- config.Window.ShowFullPathInTitle)
        config.Load()

        let defaultPath = config.DefaultPath |> Path.Parse |> Option.defaultValue Path.Root
        let startupPath =
            startupOptions.StartupPath |> Option.defaultValue (
                match config.StartupPath with
                | RestorePrevious -> config.PreviousPath
                | DefaultPath -> config.DefaultPath)
        model.Path <- defaultPath
        model.WindowLocation <- startupOptions.Location |> Option.defaultValue (config.Window.Left, config.Window.Top)
        model.WindowSize <- startupOptions.Size |> Option.defaultValue (config.Window.Width, config.Window.Height)
        openUserPath startupPath model

    module Navigation =
        let openPath getNodes (showHidden: bool) path select (model: MainModel) =
            try
                let sortField, sortDesc = model.Sort
                let sorter = SortField.SortByTypeThen sortField sortDesc
                let nodes = getNodes showHidden path |> sorter
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
                        |> Option.defaultValue keepCursor
                    | KeepSelect -> keepCursor)
                model.Status <- None
            with ex -> model.Status <- Some <| StatusType.fromExn "open path" ex

        let openUserPath getNode openPath pathStr (model: MainModel) =
            match Path.Parse pathStr with
            | Some path ->
                match getNode path with
                | Some node when node.Type = File ->
                    openPath path.Parent (SelectName node.Name) model
                | _ ->
                    openPath path KeepSelect model
            | None -> model.Status <- Some <| MainStatus.invalidPath pathStr

        let openSelected openPath openFile (model: MainModel) =
            let node = model.SelectedNode
            match node.Type with
            | Folder | Drive | NetHost | NetShare ->
                openPath node.Path KeepSelect model
            | File ->
                try
                    openFile node.Path
                    model.Status <- Some <| MainStatus.openFile node.Name
                with ex ->
                    model.Status <- Some <| MainStatus.couldNotOpenFile node.Name ex.Message
            | _ -> ()

        let back openPath (model: MainModel) =
            match model.BackStack with
            | (path, cursor) :: backTail ->
                let newForwardStack = (model.Path, model.Cursor) :: model.ForwardStack
                openPath path (SelectIndex cursor) model
                model.BackStack <- backTail
                model.ForwardStack <- newForwardStack
            | [] -> ()

        let forward openPath (model: MainModel) =
            match model.ForwardStack with
            | (path, cursor) :: forwardTail ->
                openPath path (SelectIndex cursor) model
                model.ForwardStack <- forwardTail
            | [] -> ()

        let sortList refresh field (model: MainModel) =
            let desc =
                match model.Sort with
                | f, desc when f = field -> not desc
                | _ -> field = Modified
            model.Sort <- field, desc
            refresh model
            model.Status <- Some <| MainStatus.sort field desc

    module Cursor =
        let private moveCursorToNext predicate reverse (model: MainModel) =
            let indexed = model.Nodes |> List.mapi (fun i n -> (i, n))
            let c = model.Cursor
            let items =
                if reverse then
                    Seq.append indexed.[c..] indexed.[0..(c-1)]
                    |> Seq.rev
                else
                    Seq.append indexed.[(c+1)..] indexed.[0..c]
            items
            |> Seq.filter (snd >> predicate)
            |> Seq.tryHead
            |> Option.iter (fun (i, n) -> model.SetCursor i)

        let find caseSensitive char (model: MainModel) =
            model.LastFind <- Some (caseSensitive, char)
            model.Status <- Some <| MainStatus.find caseSensitive char
            let lower = System.Char.ToLower
            let equals =
                if caseSensitive then (=)
                else (fun a b -> lower a = lower b)
            moveCursorToNext (fun n -> equals n.Name.[0] char) false model

        let findNext (model: MainModel) =
            model.LastFind |> Option.iter (fun (cs, c) -> find cs c model)

        let parseSearch (searchInput: string) =
            match searchInput.Split('/') with
            | [| search |] -> Ok (search, None)
            | [| search; switches |] ->
                let parsed =
                    switches
                    |> Seq.map (function
                        | 'c' -> Ok true
                        | 'i' -> Ok false
                        | c -> Error <| MainStatus.invalidSearchSwitch c)
                    |> Seq.toList
                let ok = parsed |> Seq.tryPick (function
                                                | Ok cs -> Some cs
                                                | _ -> None)
                let err = parsed |> Seq.tryPick (function
                                                 | Error e -> Some e
                                                 | _ -> None)
                match ok, err with
                | _, Some e -> Error e
                | cs, _ -> Ok (search, cs)
            | _ -> Error MainStatus.invalidSearchSlash

        let search caseSensitive searchStr reverse (model: MainModel) =
            let search = if searchStr <> "" then Some (caseSensitive, searchStr) else None
            let options = if caseSensitive then RegexOptions.None else RegexOptions.IgnoreCase

            let searchStatus () =
                let matches = model.Nodes |> Seq.filter (fun n -> n.IsSearchMatch) |> Seq.length
                MainStatus.search matches caseSensitive searchStr

            // if search is different, update node flags
            if model.Status <> Some (searchStatus()) then
                let cursor = model.Cursor
                model.Nodes <- model.Nodes |> List.map (fun n ->
                    let isMatch = search |> Option.exists (fun (cs, s) -> Regex.IsMatch(n.Name, s, options))
                    if isMatch && not n.IsSearchMatch then { n with IsSearchMatch = true }
                    else if not isMatch && n.IsSearchMatch then { n with IsSearchMatch = false }
                    else n)
                model.Cursor <- cursor
                model.Status <- search |> Option.map (fun _ -> searchStatus())

            if search.IsSome then
                model.LastSearch <- search

            moveCursorToNext (fun n -> n.IsSearchMatch) reverse model

        let searchNext reverse (model: MainModel) =
            model.LastSearch |> Option.iter (fun (cs, s) -> search cs s reverse model)

    module Action =
        let private runAsync (f: unit -> 'a) = f |> Task.Run |> Async.AwaitTask

        let private performedAction action (model: MainModel) =
            model.UndoStack <- action :: model.UndoStack
            model.RedoStack <- []
            model.Status <- Some <| MainStatus.actionComplete action model.PathFormat

        let private setInputSelection (model: MainModel) cursorPos =
            let fullName = model.InputText
            let name, ext = Path.SplitName fullName
            model.InputTextSelection <-
                match cursorPos with
                | Begin -> (0, 0)
                | EndName -> (name.Length, 0)
                | End -> (fullName.Length, 0)
                | ReplaceName -> (0, name.Length)
                | ReplaceAll -> (0, fullName.Length)

        let startInput getNode (inputMode: InputMode) (model: MainModel) =
            let allowed, errorStatus =
                match inputMode with
                | Input CreateFile
                | Input CreateFolder ->
                    let allowed =
                        getNode model.Path
                        |> Option.map (fun n -> n.Type.CanCreateIn)
                        |> Option.defaultValue false
                    (allowed, Some MainStatus.cannotPutHere)
                | Input (Rename _)
                | Confirm Delete ->
                    (model.SelectedNode.Type.CanModify, None)
                | _ -> (true, None)
            if allowed then
                let text, pos =
                    match inputMode with
                    | Input (Rename pos) -> model.SelectedNode.Name, (Some pos)
                    | _ -> "", None
                model.InputMode <- Some inputMode
                model.InputText <- text
                pos |> Option.iter (setInputSelection model)
            else
                errorStatus |> Option.iter (fun s -> model.Status <- Some s)

        let create getNode fsCreate openPath nodeType name (model: MainModel) =
            try
                let createPath = model.Path.Join name
                match getNode createPath with
                | None ->
                    fsCreate nodeType createPath
                    openPath model.Path (SelectName name) model
                    model |> performedAction (CreatedItem model.SelectedNode)
                | Some existing ->
                    openPath model.Path (SelectName existing.Name) model
                    model.Status <- Some <| MainStatus.cannotCreateAlreadyExists nodeType name existing.IsHidden
            with ex ->
                let action = CreatedItem { Path = model.Path; Name = name; Type = nodeType;
                                           Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }
                model |> MainStatus.setActionExceptionStatus action ex

        let undoCreate isEmpty delete refresh node (model: MainModel) = async {
            try
                if isEmpty node.Path then
                    model.Status <- Some <| MainStatus.undoingCreate node
                    do! runAsync (fun () -> delete node.Path)
                    if model.Path = node.Path.Parent then
                        refresh model
                else
                    model.Status <- Some <| MainStatus.cannotUndoNonEmptyCreated node
            with ex ->
                let action = DeletedItem (node, true)
                model |> MainStatus.setActionExceptionStatus action ex
        }

        let rename getNode move openPath node newName (model: MainModel) =
            if node.Type.CanModify then
                let action = RenamedItem (node, newName)
                try
                    let newPath = node.Path.Parent.Join newName
                    let existing = if Str.equalsIgnoreCase node.Name newName then None
                                   else getNode newPath
                    match existing with
                    | Some existingNode ->
                        model.Status <- Some <| MainStatus.cannotRenameAlreadyExists node.Type newName existingNode.IsHidden
                    | None ->
                        move node.Path newPath
                        openPath model.Path (SelectName newName) model
                        model |> performedAction action
                with ex -> model |> MainStatus.setActionExceptionStatus action ex

        let undoRename getNode move openPath oldNode currentName (model: MainModel) =
            let parentPath = oldNode.Path.Parent
            let currentPath = parentPath.Join currentName
            try
                let existing = if Str.equalsIgnoreCase oldNode.Name currentName then None
                               else getNode oldNode.Path
                match existing with
                | Some existingNode ->
                    model.Status <- Some <| MainStatus.cannotRenameAlreadyExists oldNode.Type oldNode.Name existingNode.IsHidden
                | None ->
                    move currentPath oldNode.Path
                    openPath parentPath (SelectName oldNode.Name) model
            with ex ->
                let node = { oldNode with Name = currentName; Path = currentPath }
                let action = RenamedItem (node, oldNode.Name)
                model |> MainStatus.setActionExceptionStatus action ex

        let registerItem (config: Config) action (model: MainModel) =
            if model.SelectedNode.Type.CanModify then
                model.YankRegister <- Some (model.SelectedNode, action)
                model.Status <- None

        let getCopyName name i =
            let (nameNoExt, ext) = Path.SplitName name
            let number = if i = 0 then "" else sprintf " %i" (i+1)
            sprintf "%s (copy%s)%s" nameNoExt number ext

        let putItem (config: Config) getNode move copy openPath overwrite node putAction (model: MainModel) = async {
            match getNode model.Path with
            | Some container when container.Type.CanCreateIn ->
                let sameFolder = node.Path.Parent = model.Path
                if putAction = Move && sameFolder then
                    model.Status <- Some MainStatus.cannotMoveToSameFolder
                else
                    let newName =
                        if putAction = Copy && sameFolder then
                            let exists name = Option.isSome (getNode (model.Path.Join name))
                            Seq.init 99 (getCopyName node.Name) |> Seq.find (not << exists)
                        else
                            node.Name
                    let newPath = model.Path.Join newName
                    match getNode newPath with
                    | Some existing when not overwrite ->
                        // refresh node list to make sure we can see the existing file
                        let showHidden = config.ShowHidden || existing.IsHidden
                        openPath showHidden model.Path (SelectName existing.Name) model
                        model.InputMode <- Some (Confirm (Overwrite (putAction, node, existing)))
                    | _ ->
                        let fileSysAction, action =
                            match putAction with
                            | Move -> (move, MovedItem (node, newPath))
                            | Copy -> (copy, CopiedItem (node, newPath))
                        try
                            model.Status <- MainStatus.runningAction action model.PathFormat
                            do! runAsync (fun () -> fileSysAction node.Path newPath)
                            openPath config.ShowHidden model.Path (SelectName newName) model
                            model |> performedAction action
                        with ex -> model |> MainStatus.setActionExceptionStatus action ex
            | _ -> model.Status <- Some MainStatus.cannotPutHere
        }

        let put (config: Config) getNode move copy openPath overwrite (model: MainModel) = async {
            match model.YankRegister with
            | None -> ()
            | Some (node, putAction) ->
                do! putItem config getNode move copy openPath overwrite node putAction model
                if not model.HasErrorStatus && model.InputMode.IsNone then
                    model.YankRegister <- None
        }

        let undoMove getNode move openPath node currentPath (model: MainModel) = async {
            match getNode node.Path with
            | Some _ ->
                // TODO: prompt for overwrite here?
                model.Status <- Some <| MainStatus.cannotUndoMoveToExisting node
            | None ->
                try
                    model.Status <- Some <| MainStatus.undoingMove node
                    do! runAsync (fun () -> move currentPath node.Path)
                    openPath node.Path.Parent (SelectName node.Name) model
                with ex ->
                    let from = { node with Path = currentPath; Name = currentPath.Name }
                    let action = MovedItem (from, node.Path)
                    model |> MainStatus.setActionExceptionStatus action ex
        }

        let undoCopy getNode fsDelete fsRecycle refresh node (currentPath: Path) (model: MainModel) = async {
            let mutable isDeletionPermanent = false
            try
                let copyModified = getNode currentPath |> Option.bind (fun n -> n.Modified)
                isDeletionPermanent <-
                    match node.Modified, copyModified with
                    | Some orig, Some copy when orig = copy -> true
                    | _ -> false
                let fileSysFunc = if isDeletionPermanent then fsDelete else fsRecycle
                model.Status <- Some <| MainStatus.undoingCopy node isDeletionPermanent
                do! runAsync (fun () -> fileSysFunc currentPath)
                if model.Path = currentPath.Parent then
                    refresh model
            with e ->
                let action = DeletedItem ({ node with Path = currentPath }, isDeletionPermanent)
                model |> MainStatus.setActionExceptionStatus action e
        }

        let recycle isRecyclable delete (model: MainModel) = async {
            let node = model.SelectedNode
            if node.Type.CanModify then
                model.Status <- Some <| MainStatus.checkingIsRecyclable
                try
                    let! canRecycle = runAsync (fun () -> isRecyclable node.Path)
                    if canRecycle then
                        do! delete node false model
                    else
                        model.Status <- Some <| MainStatus.cannotRecycle node
                with e -> model |> MainStatus.setActionExceptionStatus (DeletedItem (node, false)) e
        }

        let delete fsDelete fsRecycle refresh node permanent (model: MainModel) = async {
            if node.Type.CanModify then
                let action = DeletedItem (node, permanent)
                try
                    let fileSysFunc = if permanent then fsDelete else fsRecycle
                    model.Status <- MainStatus.runningAction action model.PathFormat
                    do! runAsync (fun () -> fileSysFunc node.Path)
                    refresh model
                    model |> performedAction action
                with e -> model |> MainStatus.setActionExceptionStatus action e
        }


type MainController(fileSys: FileSystemService,
                    settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>,
                    getScreenBounds: unit -> Rectangle,
                    config: Config,
                    startupOptions) =
    // TODO: use a property on the model for this, perhaps the Status?
    let mutable taskRunning = false

    interface IController<MainEvents, MainModel> with
        member this.InitModel model = MainLogic.initModel config fileSys.GetNode this.OpenUserPath startupOptions model
        member this.Dispatcher = this.LockingDispatcher

    member this.LockingDispatcher evt : EventHandler<MainModel> =
        match this.Dispatcher evt with
        | Sync handler -> Sync (fun m -> if not taskRunning then handler m)
        | Async handler ->
            Async (fun m -> async {
                if not taskRunning then
                    taskRunning <- true
                    do! handler m
                    taskRunning <- false
            })

    member this.Dispatcher = function
        | CursorUp -> Sync (fun m -> m.SetCursor (m.Cursor - 1))
        | CursorUpHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor - m.HalfPageScroll))
        | CursorDown -> Sync (fun m -> m.SetCursor (m.Cursor + 1))
        | CursorDownHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor + m.HalfPageScroll))
        | CursorToFirst -> Sync (fun m -> m.SetCursor 0)
        | CursorToLast -> Sync (fun m -> m.SetCursor (m.Nodes.Length - 1))
        | OpenPath p -> Sync (this.OpenUserPath p)
        | OpenSelected -> Sync (MainLogic.Navigation.openSelected this.OpenPath fileSys.OpenFile)
        | OpenParent -> Sync (fun m -> this.OpenPath m.Path.Parent (SelectName m.Path.Name) m)
        | Back -> Sync (MainLogic.Navigation.back this.OpenPath)
        | Forward -> Sync (MainLogic.Navigation.forward this.OpenPath)
        | Refresh -> Sync this.Refresh
        | Undo -> Async this.Undo
        | Redo -> Async this.Redo
        | StartPrompt promptType -> Sync (MainLogic.Action.startInput fileSys.GetNode (Prompt promptType))
        | StartConfirm confirmType -> Sync (MainLogic.Action.startInput fileSys.GetNode (Confirm confirmType))
        | StartInput inputType -> Sync (MainLogic.Action.startInput fileSys.GetNode (Input inputType))
        | SubmitInput -> Sync this.SubmitInput
        | InputCharTyped c -> Async (this.InputCharTyped c)
        | FindNext -> Sync MainLogic.Cursor.findNext
        | SearchNext -> Sync (MainLogic.Cursor.searchNext false)
        | SearchPrevious -> Sync (MainLogic.Cursor.searchNext true)
        | StartMove -> Sync (MainLogic.Action.registerItem config Move)
        | StartCopy -> Sync (MainLogic.Action.registerItem config Copy)
        | Put -> Async (this.Put false)
        | Recycle -> Async this.Recycle
        | SortList field -> Sync (MainLogic.Navigation.sortList this.Refresh field)
        | ToggleHidden -> Sync this.ToggleHidden
        | OpenSplitScreenWindow -> Sync this.OpenSplitScreenWindow
        | OpenSettings -> Sync this.OpenSettings
        | OpenExplorer -> Sync this.OpenExplorer
        | OpenCommandLine -> Sync this.OpenCommandLine
        | OpenWithTextEditor -> Sync this.OpenWithTextEditor
        | Exit -> Sync ignore // handled by view

    member this.OpenPath = MainLogic.Navigation.openPath fileSys.GetNodes config.ShowHidden
    member this.OpenUserPath = MainLogic.Navigation.openUserPath fileSys.GetNode this.OpenPath
    member this.Refresh model = this.OpenPath model.Path KeepSelect model

    member this.ToggleHidden model =
        config.ShowHidden <- not config.ShowHidden
        config.Save()
        this.OpenPath model.Path (SelectName model.SelectedNode.Name) model
        model.Status <- Some <| MainStatus.toggleHidden config.ShowHidden

    member this.SubmitInput model =
        let mode = model.InputMode
        model.InputMode <- None
        match mode with
        | Some (Input mode) ->
            match mode with
            | Search ->
                match MainLogic.Cursor.parseSearch model.InputText with
                | Ok (search, caseSensitive) ->
                    let caseSensitive = caseSensitive |> Option.defaultValue config.SearchCaseSensitive
                    MainLogic.Cursor.search caseSensitive search false model
                | Error msg -> model.Status <- Some msg
            | CreateFile -> this.Create File model.InputText model
            | CreateFolder -> this.Create Folder model.InputText model
            | Rename _ -> this.Rename model.SelectedNode model.InputText model
        | _ -> ()

    member this.InputCharTyped char model = async {
        let setBookmark char (path: Path) =
            let winPath = path.Format Windows
            config.SetBookmark char winPath
            config.Save()
            model.Status <- Some <| MainStatus.setBookmark char winPath
        match model.InputMode with
        | Some (Prompt mode) ->
            match mode with
            | Find caseSensitive ->
                MainLogic.Cursor.find caseSensitive char model
                model.InputMode <- None
            | GoToBookmark ->
                model.InputMode <- None
                match config.GetBookmark char with
                | Some path -> this.OpenUserPath path model
                | None -> model.Status <- Some <| MainStatus.noBookmark char
            | SetBookmark ->
                match config.GetBookmark char |> Option.bind Path.Parse with
                | Some existingPath ->
                    model.InputMode <- Some (Confirm (OverwriteBookmark (char, existingPath)))
                | None -> 
                    model.InputMode <- None
                    setBookmark char model.Path
            | DeleteBookmark ->
                model.InputMode <- None
                match config.GetBookmark char with
                | Some path ->
                    config.RemoveBookmark char
                    config.Save()
                    model.Status <- Some <| MainStatus.deletedBookmark char path
                | None -> model.Status <- Some <| MainStatus.noBookmark char
        | Some (Confirm confirmType) ->
            match char with
            | 'y' ->
                model.InputMode <- None
                match confirmType with
                | Overwrite (action, src, _) ->
                    do! this.PutItem true src action model
                    if not model.HasErrorStatus then
                        model.YankRegister <- None
                | Delete -> do! this.Delete model.SelectedNode true model
                | OverwriteBookmark (char, _) -> setBookmark char model.Path
            | 'n' ->
                model.InputMode <- None
                match confirmType with
                | Overwrite _ when not config.ShowHidden && model.Nodes |> Seq.exists (fun n -> n.IsHidden) ->
                    // if we were temporarily showing hidden files, refresh
                    this.Refresh model
                | _ -> ()
                model.Status <- Some <| MainStatus.cancelled
            | _ -> ()
        | _ -> ()
    }

    member this.Create = MainLogic.Action.create fileSys.GetNode fileSys.Create this.OpenPath
    member this.Rename = MainLogic.Action.rename fileSys.GetNode fileSys.Move this.OpenPath
    member this.Put = MainLogic.Action.put config fileSys.GetNode fileSys.Move fileSys.Copy (MainLogic.Navigation.openPath fileSys.GetNodes)
    member this.PutItem = MainLogic.Action.putItem config fileSys.GetNode fileSys.Move fileSys.Copy (MainLogic.Navigation.openPath fileSys.GetNodes)
    member this.Recycle = this.RemoveIfNetHost (MainLogic.Action.recycle fileSys.IsRecyclable this.Delete)
    member this.Delete = MainLogic.Action.delete fileSys.Delete fileSys.Recycle this.Refresh

    member private this.RemoveIfNetHost otherHandler model = async {
        if model.SelectedNode.Type = NetHost then
            config.RemoveNetHost model.SelectedNode.Name
            config.Save()
            return this.Refresh model
        else
            return! otherHandler model
    }

    member this.Undo model = async {
        match model.UndoStack with
        | action :: rest ->
            match action with
            | CreatedItem node ->
                do! MainLogic.Action.undoCreate fileSys.IsEmpty fileSys.Delete this.Refresh node model
            | RenamedItem (oldNode, curName) ->
                MainLogic.Action.undoRename fileSys.GetNode fileSys.Move this.OpenPath oldNode curName model
            | MovedItem (node, newPath) ->
                do! MainLogic.Action.undoMove fileSys.GetNode fileSys.Move this.OpenPath node newPath model
            | CopiedItem (node, newPath) ->
                do! MainLogic.Action.undoCopy fileSys.GetNode fileSys.Delete fileSys.Recycle this.Refresh node newPath model
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
                model.Status <- MainStatus.redoingAction action model.PathFormat
                do! this.PutItem false node moveOrCopy model
            | DeletedItem (node, permanent) ->
                goToPath node.Path
                model.Status <- MainStatus.redoingAction action model.PathFormat
                do! this.Delete node permanent model

            model.RedoStack <- rest
            model.Status <- Some <| MainStatus.redoAction action model.PathFormat
        | [] -> model.Status <- Some <| MainStatus.noRedoActions
    }

    member this.OpenSplitScreenWindow model =
        match Path.Parse System.AppDomain.CurrentDomain.BaseDirectory with
        | None -> model.Status <- Some <| ErrorMessage "Could not determine Koffee.exe path"
        | Some folder ->
            let mapFst f t = (fst t |> f, snd t)
            let fitRect = Rect.ofPairs model.WindowLocation (model.WindowSize |> mapFst ((*) 2))
                          |> Rect.fit (getScreenBounds())
            model.WindowLocation <- fitRect.Location
            model.WindowSize <- fitRect.Size |> mapFst (flip (/) 2)

            let left, top = model.WindowLocation
            let width, height = model.WindowSize
            let path = (folder.Join "Koffee.exe").Format Windows
            let args = sprintf "\"%s\" --location=%i,%i --size=%i,%i"
                               (model.Path.Format Windows) (left + width) top width height
            fileSys.LaunchApp path folder args

    member this.OpenExplorer model =
        fileSys.OpenExplorer model.SelectedNode
        model.Status <- Some <| MainStatus.openExplorer

    member this.OpenCommandLine model =
        if model.Path <> Path.Root then
            try
                fileSys.LaunchApp config.CommandlinePath model.Path ""
                model.Status <- Some <| MainStatus.openCommandLine model.PathFormatted
            with ex ->
                model.Status <- Some <| ErrorMessage ex.Message // TODO

    member this.OpenWithTextEditor model =
        match model.SelectedNode.Type with
        | File ->
            try
                let args = model.SelectedNode.Path.Format Windows |> sprintf "\"%s\""
                fileSys.LaunchApp config.TextEditor model.Path args
                model.Status <- Some <| MainStatus.openTextEditor model.SelectedNode.Name
            with ex ->
                model.Status <- Some <| MainStatus.couldNotOpenTextEditor ex.Message
        | _ -> ()

    member this.OpenSettings model =
        let settings = settingsFactory()
        settings.StartDialog() |> ignore

        model.PathFormat <- config.PathFormat
        model.ShowFullPathInTitle <- config.Window.ShowFullPathInTitle
        this.Refresh model
