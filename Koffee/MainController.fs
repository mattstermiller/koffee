namespace Koffee

open FSharp.Desktop.UI
open System.Text.RegularExpressions
open System.Threading.Tasks
open Koffee.ConfigExt
open Utility

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

module MainLogic =
    let syncConfig (config: Config) (model: MainModel) =
        model.PathFormat <- config.PathFormat
        model.ShowHidden <- config.ShowHidden
        model.ShowFullPathInTitle <- config.Window.ShowFullPathInTitle

    let initModel (config: Config) getNode openUserPath startupOptions isFirstInstance (model: MainModel) =
        config.Changed.Add (fun _ ->
            model.YankRegister <- config.YankRegister
                                  |> Option.bind (fun (path, action) ->
                                      match getNode path with
                                      | Ok (Some node) -> Some (node, action)
                                      | _ -> None)
            syncConfig config model
        )
        config.Load()

        let defaultPath = config.DefaultPath |> Path.Parse |> Option.defaultValue Path.Root
        let startupPath =
            startupOptions.StartupPath |> Option.defaultValue (
                match config.StartupPath with
                | RestorePrevious -> config.PreviousPath
                | DefaultPath -> config.DefaultPath)
        model.Path <- defaultPath
        model.WindowLocation <-
            startupOptions.Location
            |> Option.defaultWith (fun () ->
                if isFirstInstance then (config.Window.Left, config.Window.Top)
                else (config.Window.Left + 30, config.Window.Top + 30))
        model.WindowSize <- startupOptions.Size |> Option.defaultValue (config.Window.Width, config.Window.Height)
        openUserPath startupPath model

    let actionError actionName = Result.mapError (fun e -> ActionError (actionName, e))
    let itemActionError item pathFormat = Result.mapError (fun e -> ItemActionError (item, pathFormat, e))

    module Navigation =
        let openPath getNodes path select (model: MainModel) = result {
            let! nodes = getNodes model.ShowHidden path |> actionError "open path"
            let sortField, sortDesc = model.Sort
            let nodes = nodes |> SortField.SortByTypeThen sortField sortDesc
            if path <> model.Path then
                model.BackStack <- (model.Path, model.Cursor) :: model.BackStack
                model.ForwardStack <- []
                model.Path <- path
                model.Cursor <- 0
            let cursor = model.Cursor
            model.Nodes <- nodes
            model.SetCursor (
                match select with
                | SelectIndex index -> index
                | SelectName name ->
                    List.tryFindIndex (fun n -> n.Name = name) nodes
                    |> Option.defaultValue cursor
                | SelectNone -> cursor)
            model.Status <- None
        }

        let openUserPath getNode openPath pathStr (model: MainModel) =
            match Path.Parse pathStr with
            | Some path ->
                match getNode path with
                | Ok (Some node) when node.Type = File ->
                    openPath path.Parent (SelectName node.Name) model
                | Ok _ ->
                    openPath path SelectNone model
                | Error e -> Error <| ActionError ("open path", e)
            | None -> Error <| InvalidPath pathStr

        let openSelected openPath (os: IOperatingSystem) (model: MainModel) =
            let node = model.SelectedNode
            match node.Type with
            | Folder | Drive | NetHost | NetShare ->
                openPath node.Path SelectNone model
            | File ->
                os.OpenFile node.Path |> actionError (sprintf "open '%s'" node.Name)
                |> Result.map (fun () ->
                    model.Status <- Some <| MainStatus.openFile node.Name)
            | _ -> Ok ()

        let back openPath (model: MainModel) = result {
            match model.BackStack with
            | (path, cursor) :: backTail ->
                let newForwardStack = (model.Path, model.Cursor) :: model.ForwardStack
                do! openPath path (SelectIndex cursor) model
                model.BackStack <- backTail
                model.ForwardStack <- newForwardStack
            | [] -> ()
        }

        let forward openPath (model: MainModel) = result {
            match model.ForwardStack with
            | (path, cursor) :: forwardTail ->
                do! openPath path (SelectIndex cursor) model
                model.ForwardStack <- forwardTail
            | [] -> ()
        }

        let sortList refresh field (model: MainModel) = result {
            let desc =
                match model.Sort with
                | f, desc when f = field -> not desc
                | _ -> field = Modified
            model.Sort <- field, desc
            do! refresh model
            model.Status <- Some <| MainStatus.sort field desc
        }

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
            |> Option.iter (fst >> model.SetCursor)

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
                (Ok (search, None), switches) ||> Seq.fold (fun res c ->
                    match res, c with
                    | Ok _, c when not <| Seq.contains c "ci" -> Error <| InvalidSearchSwitch c
                    | Ok (s, None), 'c' -> Ok (s, Some true)
                    | Ok (s, None), 'i' -> Ok (s, Some false)
                    | _ -> res)
            | _ -> Error InvalidSearchSlash

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

        let startInput getNode (inputMode: InputMode) (model: MainModel) = result {
            let! allowed =
                match inputMode with
                | Input CreateFile
                | Input CreateFolder ->
                    match getNode model.Path with
                    | Ok (Some node) when node.Type.CanCreateIn -> Ok true
                    | Ok _ -> Error <| CannotPutHere
                    | Error e -> Error <| ActionError ("create item", e)
                | Input (Rename _)
                | Confirm Delete -> Ok model.SelectedNode.Type.CanModify 
                | _ -> Ok true
            if allowed then
                model.InputMode <- Some inputMode
                match inputMode with
                | Input (Rename pos) ->
                    model.InputText <- model.SelectedNode.Name
                    setInputSelection model pos
                | _ ->
                    model.InputText <- ""
        }

        let create getNode fsCreate openPath nodeType name (model: MainModel) = result {
            let createPath = model.Path.Join name
            let action = CreatedItem { Path = createPath; Name = name; Type = nodeType;
                                       Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }
            let! existing = getNode createPath |> itemActionError action model.PathFormat
            match existing with
            | None ->
                do! fsCreate nodeType createPath |> itemActionError action model.PathFormat
                do! openPath model.Path (SelectName name) model
                model |> performedAction (CreatedItem model.SelectedNode)
            | Some existing ->
                do! openPath model.Path (SelectName existing.Name) model
                return! Error <| CannotUseNameAlreadyExists ("create", nodeType, name, existing.IsHidden)
        }

        let undoCreate isEmpty delete refresh node (model: MainModel) = asyncResult {
            if isEmpty node.Path then
                model.Status <- Some <| MainStatus.undoingCreate node
                let! res = runAsync (fun () -> delete node.Path)
                do! res |> itemActionError (DeletedItem (node, true)) model.PathFormat
                if model.Path = node.Path.Parent then
                    refresh model |> ignore
            else
                return! Error <| CannotUndoNonEmptyCreated node
        }

        let rename getNode move openPath node newName (model: MainModel) = result {
            if node.Type.CanModify then
                let action = RenamedItem (node, newName)
                let newPath = node.Path.Parent.Join newName
                let! existing =
                    if Str.equalsIgnoreCase node.Name newName then Ok None
                    else getNode newPath |> itemActionError action model.PathFormat
                match existing with
                | None ->
                    do! move node.Path newPath |> itemActionError action model.PathFormat
                    do! openPath model.Path (SelectName newName) model
                    model |> performedAction action
                | Some existingNode ->
                    return! Error <| CannotUseNameAlreadyExists ("rename", node.Type, newName, existingNode.IsHidden)
            }

        let undoRename getNode move openPath oldNode currentName (model: MainModel) = result {
            let parentPath = oldNode.Path.Parent
            let currentPath = parentPath.Join currentName
            let node = { oldNode with Name = currentName; Path = currentPath }
            let action = RenamedItem (node, oldNode.Name)
            let! existing =
                if Str.equalsIgnoreCase oldNode.Name currentName then Ok None
                else getNode oldNode.Path |> itemActionError action model.PathFormat
            match existing with
            | None ->
                do! move currentPath oldNode.Path |> itemActionError action model.PathFormat
                do! openPath parentPath (SelectName oldNode.Name) model
            | Some existingNode ->
                return! Error <| CannotUseNameAlreadyExists ("rename", oldNode.Type, oldNode.Name, existingNode.IsHidden)
        }

        let registerItem action (model: MainModel) =
            if model.SelectedNode.Type.CanModify then
                model.YankRegister <- Some (model.SelectedNode, action)
                model.Status <- None

        let getCopyName name i =
            let (nameNoExt, ext) = Path.SplitName name
            let number = if i = 0 then "" else sprintf " %i" (i+1)
            sprintf "%s (copy%s)%s" nameNoExt number ext

        let putItem getNode move copy openPath overwrite node putAction (model: MainModel) = asyncResult {
            let sameFolder = node.Path.Parent = model.Path
            let! container = getNode model.Path |> actionError "put item"
            match container with
            | Some container when container.Type.CanCreateIn ->
                if putAction = Move && sameFolder then
                    return! Error CannotMoveToSameFolder
            | _ -> return! Error CannotPutHere
            let! newName =
                if putAction = Copy && sameFolder then
                    let unused name =
                        match getNode (model.Path.Join name) with
                        | Ok None -> true
                        | _ -> false
                    Seq.init 99 (getCopyName node.Name)
                    |> Seq.tryFind (unused)
                    |> Result.ofOption (TooManyCopies node.Name)
                else
                    Ok node.Name
            let newPath = model.Path.Join newName
            let fileSysAction, action =
                match putAction with
                | Move -> (move, MovedItem (node, newPath))
                | Copy -> (copy, CopiedItem (node, newPath))
            let! existing = getNode newPath |> itemActionError action model.PathFormat
            match existing with
            | Some existing when not overwrite ->
                // refresh node list to make sure we can see the existing file
                let tempShowHidden = not model.ShowHidden && existing.IsHidden
                if tempShowHidden then
                    model.ShowHidden <- true
                let res: Result<_,_> = openPath model.Path (SelectName existing.Name) model
                if tempShowHidden then
                    model.ShowHidden <- false
                do! res
                model.InputMode <- Some (Confirm (Overwrite (putAction, node, existing)))
                model.InputText <- ""
            | _ ->
                model.Status <- MainStatus.runningAction action model.PathFormat
                let! res = runAsync (fun () -> fileSysAction node.Path newPath)
                do! res |> itemActionError action model.PathFormat
                openPath model.Path (SelectName newName) model |> ignore
                model |> performedAction action
        }

        let put getNode move copy openPath overwrite (model: MainModel) = asyncResult {
            match model.YankRegister with
            | None -> ()
            | Some (node, putAction) ->
                let! res = putItem getNode move copy openPath overwrite node putAction model
                do! res
                if not model.HasErrorStatus && model.InputMode.IsNone then
                    model.YankRegister <- None
        }

        let undoMove getNode move openPath node currentPath (model: MainModel) = asyncResult {
            let from = { node with Path = currentPath; Name = currentPath.Name }
            let action = MovedItem (from, node.Path)
            let! existing = getNode node.Path |> itemActionError action model.PathFormat
            match existing with
            | Some _ ->
                // TODO: prompt for overwrite here?
                return! Error <| CannotUndoMoveToExisting node
            | None ->
                model.Status <- Some <| MainStatus.undoingMove node
                let! res = runAsync (fun () -> move currentPath node.Path)
                do! res |> itemActionError action model.PathFormat
                openPath node.Path.Parent (SelectName node.Name) model |> ignore
        }

        let undoCopy getNode fsDelete fsRecycle refresh node (currentPath: Path) (model: MainModel) = asyncResult {
            let copyModified =
                match getNode currentPath with
                | Ok (Some copy) -> copy.Modified
                | _ -> None
            let isDeletionPermanent =
                match node.Modified, copyModified with
                | Some orig, Some copy when orig = copy -> true
                | _ -> false
            let action = DeletedItem ({ node with Path = currentPath }, isDeletionPermanent)
            let fileSysFunc = if isDeletionPermanent then fsDelete else fsRecycle
            model.Status <- Some <| MainStatus.undoingCopy node isDeletionPermanent
            let! res = runAsync (fun () -> fileSysFunc currentPath)
            do! res |> itemActionError action model.PathFormat
            if model.Path = currentPath.Parent then
                refresh model |> ignore
        }

        let delete fsDelete fsRecycle refresh node permanent (model: MainModel) = asyncResult {
            if node.Type.CanModify then
                let action = DeletedItem (node, permanent)
                let fileSysFunc = if permanent then fsDelete else fsRecycle
                model.Status <- MainStatus.runningAction action model.PathFormat
                let! res = runAsync (fun () -> fileSysFunc node.Path)
                do! res |> itemActionError action model.PathFormat
                refresh model |> ignore
                model |> performedAction action
        }


type MainController(fsReader: IFileSystemReader,
                    fsWriter: IFileSystemWriter,
                    operatingSystem: IOperatingSystem,
                    settingsFactory: unit -> Mvc<SettingsEvents, SettingsModel>,
                    getScreenBounds: unit -> Rectangle,
                    closeWindow: unit -> unit,
                    config: Config,
                    keyBindings: (KeyCombo * MainEvents) list,
                    startupOptions) =
    // TODO: use a property on the model for this, perhaps the Status?
    let mutable taskRunning = false

    let applyResult (model: MainModel) = function
        | Ok () -> ()
        | Error e -> model.SetError e

    let resultHandler handler =
        Sync (fun model -> handler model |> applyResult model)

    let asyncResultHandler handler =
        Async (fun model -> async {
            let! res = handler model
            res |> applyResult model
        })

    interface IController<MainEvents, MainModel> with
        member this.InitModel model =
            let isFirst =
                System.Diagnostics.Process.GetProcesses()
                |> Seq.where (fun p -> Str.equalsIgnoreCase p.ProcessName "koffee")
                |> Seq.length
                |> (=) 1
            MainLogic.initModel config fsReader.GetNode this.OpenUserPath startupOptions isFirst model
            |> applyResult model
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
        | KeyPress (chord, handler) -> Async <| this.KeyPress chord handler.Handle
        | CursorUp -> Sync (fun m -> m.SetCursor (m.Cursor - 1))
        | CursorUpHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor - m.HalfPageScroll))
        | CursorDown -> Sync (fun m -> m.SetCursor (m.Cursor + 1))
        | CursorDownHalfPage -> Sync (fun m -> m.SetCursor (m.Cursor + m.HalfPageScroll))
        | CursorToFirst -> Sync (fun m -> m.SetCursor 0)
        | CursorToLast -> Sync (fun m -> m.SetCursor (m.Nodes.Length - 1))
        | OpenPath p -> resultHandler (this.OpenUserPath p)
        | OpenSelected -> resultHandler (MainLogic.Navigation.openSelected this.OpenPath operatingSystem)
        | OpenParent -> resultHandler (fun m -> this.OpenPath m.Path.Parent (SelectName m.Path.Name) m)
        | Back -> resultHandler (MainLogic.Navigation.back this.OpenPath)
        | Forward -> resultHandler (MainLogic.Navigation.forward this.OpenPath)
        | Refresh -> resultHandler this.Refresh
        | Undo -> asyncResultHandler this.Undo
        | Redo -> asyncResultHandler this.Redo
        | StartPrompt promptType -> resultHandler (MainLogic.Action.startInput fsReader.GetNode (Prompt promptType))
        | StartConfirm confirmType -> resultHandler (MainLogic.Action.startInput fsReader.GetNode (Confirm confirmType))
        | StartInput inputType -> resultHandler (MainLogic.Action.startInput fsReader.GetNode (Input inputType))
        | SubmitInput -> resultHandler this.SubmitInput
        | InputCharTyped c -> asyncResultHandler (this.InputCharTyped c)
        | FindNext -> Sync MainLogic.Cursor.findNext
        | SearchNext -> Sync (MainLogic.Cursor.searchNext false)
        | SearchPrevious -> Sync (MainLogic.Cursor.searchNext true)
        | StartMove -> Sync (MainLogic.Action.registerItem Move)
        | StartCopy -> Sync (MainLogic.Action.registerItem Copy)
        | Put -> asyncResultHandler (this.Put false)
        | Recycle -> asyncResultHandler this.Recycle
        | SortList field -> resultHandler (MainLogic.Navigation.sortList this.Refresh field)
        | ToggleHidden -> resultHandler this.ToggleHidden
        | OpenSplitScreenWindow -> resultHandler this.OpenSplitScreenWindow
        | OpenSettings -> resultHandler this.OpenSettings
        | OpenExplorer -> Sync this.OpenExplorer
        | OpenCommandLine -> resultHandler this.OpenCommandLine
        | OpenWithTextEditor -> resultHandler this.OpenWithTextEditor
        | Exit -> Sync (ignore >> closeWindow)

    member this.KeyPress chord handleKey model = async {
        let event =
            if chord = (ModifierKeys.None, Key.Escape) then
                handleKey ()
                if model.InputMode.IsSome then
                    model.InputMode <- None
                else if not model.KeyCombo.IsEmpty then
                    model.KeyCombo <- []
                else
                    model.Status <- None
                None
            else if chord = (ModifierKeys.Control, Key.C) then
                handleKey () // prevent crash due to bug in WPF datagrid
                None
            else
                let keyCombo = List.append model.KeyCombo [chord]
                match KeyBinding.getMatch keyBindings keyCombo with
                | KeyBinding.Match newEvent ->
                    handleKey ()
                    model.KeyCombo <- []
                    Some newEvent
                | KeyBinding.PartialMatch ->
                    handleKey ()
                    model.KeyCombo <- keyCombo
                    None
                | KeyBinding.NoMatch ->
                    model.KeyCombo <- []
                    None
        match event with
        | Some e ->
            match this.Dispatcher e with
            | Sync handler -> handler model
            | Async handler -> do! handler model
        | None -> ()
    }

    member this.OpenPath = MainLogic.Navigation.openPath fsReader.GetNodes
    member this.OpenUserPath = MainLogic.Navigation.openUserPath fsReader.GetNode this.OpenPath
    member this.Refresh model = this.OpenPath model.Path SelectNone model

    member this.Create = MainLogic.Action.create fsReader.GetNode fsWriter.Create this.OpenPath
    member this.Rename = MainLogic.Action.rename fsReader.GetNode fsWriter.Move this.OpenPath
    member this.Put = MainLogic.Action.put fsReader.GetNode fsWriter.Move fsWriter.Copy (MainLogic.Navigation.openPath fsReader.GetNodes)
    member this.PutItem = MainLogic.Action.putItem fsReader.GetNode fsWriter.Move fsWriter.Copy (MainLogic.Navigation.openPath fsReader.GetNodes)
    member this.Delete = MainLogic.Action.delete fsWriter.Delete fsWriter.Recycle this.Refresh

    member this.ToggleHidden model = result {
        model.ShowHidden <- not model.ShowHidden
        do! this.OpenPath model.Path (SelectName model.SelectedNode.Name) model
        model.Status <- Some <| MainStatus.toggleHidden model.ShowHidden
    }

    member this.SubmitInput model =
        let mode = model.InputMode
        model.InputMode <- None
        match mode with
        | Some (Input mode) ->
            match mode with
            | Search ->
                MainLogic.Cursor.parseSearch model.InputText
                |> Result.map (fun (search, caseSensitive) ->
                    let caseSensitive = caseSensitive |> Option.defaultValue config.SearchCaseSensitive
                    MainLogic.Cursor.search caseSensitive search false model)
            | CreateFile -> this.Create File model.InputText model
            | CreateFolder -> this.Create Folder model.InputText model
            | Rename _ -> this.Rename model.SelectedNode model.InputText model
        | _ -> Ok ()

    member this.InputCharTyped char model = asyncResult {
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
                | Some path -> return! this.OpenUserPath path model
                | None -> model.Status <- Some <| MainStatus.noBookmark char
            | SetBookmark ->
                match config.GetBookmark char |> Option.bind Path.Parse with
                | Some existingPath ->
                    model.InputMode <- Some (Confirm (OverwriteBookmark (char, existingPath)))
                    model.InputText <- ""
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
                    let! res = this.PutItem true src action model
                    do! res
                    model.YankRegister <- None
                | Delete ->
                    let! res = this.Delete model.SelectedNode true model
                    do! res
                | OverwriteBookmark (char, _) -> setBookmark char model.Path
            | 'n' ->
                model.InputMode <- None
                model.Status <- Some <| MainStatus.cancelled
                match confirmType with
                | Overwrite _ when not model.ShowHidden && model.Nodes |> Seq.exists (fun n -> n.IsHidden) ->
                    // if we were temporarily showing hidden files, refresh
                    return! this.Refresh model
                | _ -> ()
            | _ -> ()
        | _ -> ()
    }

    member this.Recycle model = asyncResult {
        if model.SelectedNode.Type = NetHost then
            let host = model.SelectedNode.Name
            config.RemoveNetHost host
            config.Save()
            do! this.Refresh model
            model.Status <- Some <| MainStatus.removedNetworkHost host
        else
            let! res = this.Delete model.SelectedNode false model
            do! res
    }

    member this.Undo model = asyncResult {
        match model.UndoStack with
        | action :: rest ->
            model.UndoStack <- rest
            match action with
            | CreatedItem node ->
                let! res = MainLogic.Action.undoCreate fsReader.IsEmpty fsWriter.Delete this.Refresh node model
                do! res
            | RenamedItem (oldNode, curName) ->
                do! MainLogic.Action.undoRename fsReader.GetNode fsWriter.Move this.OpenPath oldNode curName model
            | MovedItem (node, newPath) ->
                let! res = MainLogic.Action.undoMove fsReader.GetNode fsWriter.Move this.OpenPath node newPath model
                do! res
            | CopiedItem (node, newPath) ->
                let! res = MainLogic.Action.undoCopy fsReader.GetNode fsWriter.Delete fsWriter.Recycle this.Refresh node newPath model
                do! res
            | DeletedItem (node, permanent) ->
                return! Error <| CannotUndoDelete (permanent, node)
            model.RedoStack <- action :: model.RedoStack
            model.Status <- Some <| MainStatus.undoAction action model.PathFormat
        | [] -> return! Error NoUndoActions
    }

    member this.Redo model = asyncResult {
        match model.RedoStack with
        | action :: rest ->
            model.RedoStack <- rest
            let goToPath (nodePath: Path) =
                let path = nodePath.Parent
                if path <> model.Path then
                    this.OpenPath path SelectNone model
                else Ok ()
            match action with
            | CreatedItem node ->
                do! goToPath node.Path
                do! this.Create node.Type node.Name model
            | RenamedItem (node, newName) ->
                do! goToPath node.Path
                do! this.Rename node newName model
            | MovedItem (node, newPath)
            | CopiedItem (node, newPath) ->
                do! goToPath newPath
                let moveOrCopy =
                    match action with
                    | MovedItem _ -> Move
                    | _ -> Copy
                model.Status <- MainStatus.redoingAction action model.PathFormat
                let! res = this.PutItem false node moveOrCopy model
                do! res
            | DeletedItem (node, permanent) ->
                do! goToPath node.Path
                model.Status <- MainStatus.redoingAction action model.PathFormat
                let! res = this.Delete node permanent model
                do! res
            model.RedoStack <- rest
            model.Status <- Some <| MainStatus.redoAction action model.PathFormat
        | [] -> return! Error NoRedoActions
    }

    member this.OpenSplitScreenWindow model = result {
        let mapFst f t = (fst t |> f, snd t)
        let fitRect = Rect.ofPairs model.WindowLocation (model.WindowSize |> mapFst ((*) 2))
                      |> Rect.fit (getScreenBounds())
        model.WindowLocation <- fitRect.Location
        model.WindowSize <- fitRect.Size |> mapFst (flip (/) 2)

        let left, top = model.WindowLocation
        let width, height = model.WindowSize
        let left = left + width
        let args = sprintf "\"%s\" --location=%i,%i --size=%i,%i"
                           (model.Path.Format Windows) left top width height

        let! koffeePath = Path.Parse (System.Reflection.Assembly.GetExecutingAssembly().Location)
                          |> Result.ofOption CouldNotFindKoffeeExe
        let folder = koffeePath.Parent
        do! operatingSystem.LaunchApp (koffeePath.Format Windows) folder args
            |> Result.mapError (fun e -> CouldNotOpenApp ("Koffee", e))
    }

    member this.OpenExplorer model =
        operatingSystem.OpenExplorer model.SelectedNode
        model.Status <- Some <| MainStatus.openExplorer

    member this.OpenCommandLine model = result {
        if model.Path <> Path.Root then
            do! operatingSystem.LaunchApp config.CommandlinePath model.Path ""
                |> Result.mapError (fun e -> CouldNotOpenApp ("Commandline tool", e))
            model.Status <- Some <| MainStatus.openCommandLine model.PathFormatted
    }

    member this.OpenWithTextEditor model = result {
        match model.SelectedNode.Type with
        | File ->
            let args = model.SelectedNode.Path.Format Windows |> sprintf "\"%s\""
            do! operatingSystem.LaunchApp config.TextEditor model.Path args
                |> Result.mapError (fun e -> CouldNotOpenApp ("Text Editor", e))
            model.Status <- Some <| MainStatus.openTextEditor model.SelectedNode.Name
        | _ -> ()
    }

    member this.OpenSettings model = result {
        settingsFactory().StartDialog() |> ignore

        MainLogic.syncConfig config model
        do! this.Refresh model
    }
