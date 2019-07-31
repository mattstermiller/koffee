module Koffee.MainLogic

open System.Text.RegularExpressions
open System.Threading.Tasks
open FSharp.Control
open VinylUI
open Acadian.FSharp

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

let loadConfig (fsReader: IFileSystemReader) (config: Config) model =
    { model with
        YankRegister =
            config.YankRegister |> Option.bind (fun (path, action) ->
                match fsReader.GetNode path with
                | Ok (Some node) -> Some (node, action)
                | _ -> None
            )
        PathFormat = config.PathFormat
        ShowHidden = config.ShowHidden
        ShowFullPathInTitle = config.Window.ShowFullPathInTitle
    }

let actionError actionName = Result.mapError (fun e -> ActionError (actionName, e))
let itemActionError item pathFormat = Result.mapError (fun e -> ItemActionError (item, pathFormat, e))

module Nav =
    let openPath (fsReader: IFileSystemReader) path select model = result {
        let! nodes = fsReader.GetNodes model.ShowHidden path |> actionError "open path"
        let nodes = nodes |> SortField.SortByTypeThen model.Sort
        let model =
            if path <> model.Location then
                { model.WithLocation path with
                    BackStack = (model.Location, model.Cursor) :: model.BackStack
                    ForwardStack = []
                    Cursor = 0
                }
            else
                model.WithLocation path
        let cursor =
            match select with
            | SelectIndex index -> index
            | SelectName name -> List.tryFindIndex (fun n -> n.Name = name) nodes |? model.Cursor
            | SelectNone -> model.Cursor
        return
            { model with
                Nodes = nodes
                Status = None
            }.WithCursor cursor
    }

    let openUserPath (fsReader: IFileSystemReader) pathStr model =
        match Path.Parse pathStr with
        | Some path ->
            match fsReader.GetNode path with
            | Ok (Some node) when node.Type = File ->
                openPath fsReader path.Parent (SelectName node.Name) model
            | Ok _ ->
                openPath fsReader path SelectNone model
            | Error e -> Error <| ActionError ("open path", e)
        | None -> Error <| InvalidPath pathStr

    let openInputPath fsReader (evtHandler: EvtHandler) model = result {
        let! model = openUserPath fsReader model.LocationInput model
        evtHandler.Handle ()
        return model
    }

    let openSelected fsReader (os: IOperatingSystem) (model: MainModel) =
        let node = model.SelectedNode
        match node.Type with
        | Folder | Drive | NetHost | NetShare ->
            openPath fsReader node.Path SelectNone model
        | File ->
            let openError e = e |> actionError (sprintf "open '%s'" node.Name)
            let shortcutFolder = result {
                let! targetPath =
                    if node.Path.Extension |> String.equalsIgnoreCase "lnk" then
                        fsReader.GetShortcutTarget node.Path |> openError
                        |> Result.map Path.Parse
                    else
                        Ok None
                match targetPath with
                | Some targetPath ->
                    let! target =
                        fsReader.GetNode targetPath |> openError
                        |> Result.bind (Result.ofOption (ShortcutTargetMissing (targetPath.Format model.PathFormat)))
                    return if target.Type = Folder then Some target.Path else None
                | None ->
                    return None
            }
            shortcutFolder
            |> Result.bind (function
                | Some shortcutFolder ->
                    openPath fsReader shortcutFolder SelectNone model
                | None ->
                    os.OpenFile node.Path |> openError
                    |> Result.map (fun () ->
                        { model with Status = Some <| MainStatus.openFile node.Name }
                    )
            )
        | _ -> Ok model

    let openParent fsReader model =
        openPath fsReader model.Location.Parent (SelectName model.Location.Name) model

    let refresh fsReader model =
        openPath fsReader model.Location SelectNone model
        |> Result.map (fun newModel ->
            if model.Status.IsSome then
                { newModel with Status = model.Status }
            else newModel
        )

    let back fsReader model = result {
        match model.BackStack with
        | (path, cursor) :: backTail ->
            let newForwardStack = (model.Location, model.Cursor) :: model.ForwardStack
            let! model = openPath fsReader path (SelectIndex cursor) model
            return
                { model with
                    BackStack = backTail
                    ForwardStack = newForwardStack
                }
        | [] -> return model
    }

    let forward fsReader model = result {
        match model.ForwardStack with
        | (path, cursor) :: forwardTail ->
            let! model = openPath fsReader path (SelectIndex cursor) model
            return { model with ForwardStack = forwardTail }
        | [] -> return model
    }

    let sortList fsReader field model = result {
        let desc =
            match model.Sort with
            | f, desc when f = field -> not desc
            | _ -> field = Modified
        let select = if model.Cursor = 0 then SelectNone else SelectName model.SelectedNode.Name
        let! model =
            { model with Sort = field, desc }
            |> openPath fsReader model.Location select
        return { model with Status = Some <| MainStatus.sort field desc }
    }

    let toggleHidden fsReader model = result {
        let! model =
            { model with ShowHidden = not model.ShowHidden }
            |> openPath fsReader model.Location (SelectName model.SelectedNode.Name)
        return { model with Status = Some <| MainStatus.toggleHidden model.ShowHidden }
    }

module Cursor =
    let private moveCursorTo next reverse predicate model =
        let rotate offset (list: _ list) = list.[offset..] @ list.[0..(offset-1)]
        let indexed = model.Nodes |> List.indexed
        let items =
            if reverse then
                indexed |> rotate (model.Cursor + (if next then 0 else 1)) |> List.rev
            else
                indexed |> rotate (model.Cursor + (if next then 1 else 0))
        let cursor =
            items
            |> List.filter (snd >> predicate)
            |> List.tryHead
            |> Option.map fst
        match cursor with
        | Some cursor -> model.WithCursor cursor
        | None -> model

    let find prefix model =
        { model with LastFind = Some prefix }
        |> moveCursorTo false false (fun n -> n.Name |> String.startsWithIgnoreCase prefix)

    let findNext model =
        match model.LastFind with
        | Some prefix ->
            { model with Status = Some <| MainStatus.find prefix }
            |> moveCursorTo true false (fun n -> n.Name |> String.startsWithIgnoreCase prefix)
        | None -> model

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

    let search caseSensitive searchStr reverse model =
        let search = if searchStr <> "" then Some (caseSensitive, searchStr) else None
        let options = if caseSensitive then RegexOptions.None else RegexOptions.IgnoreCase

        let searchStatus nodes =
            let matches = nodes |> List.filter (fun n -> n.IsSearchMatch) |> List.length
            MainStatus.search matches caseSensitive searchStr

        // if search is different, update node flags
        let model =
            if model.Status <> Some (searchStatus model.Nodes) then
                let nodes = model.Nodes |> List.map (fun n ->
                    let isMatch = search |> Option.exists (fun (cs, s) -> Regex.IsMatch(n.Name, s, options))
                    if isMatch && not n.IsSearchMatch then { n with IsSearchMatch = true }
                    else if not isMatch && n.IsSearchMatch then { n with IsSearchMatch = false }
                    else n
                )
                { model with
                    Nodes = nodes
                    Status = search |> Option.map (fun _ -> searchStatus nodes)
                }
            else
                model

        { model with
            LastSearch = search |> Option.orElse model.LastSearch
        } |> moveCursorTo true reverse (fun n -> n.IsSearchMatch)

    let searchNext reverse model =
        match model.LastSearch with
        | Some (cs, s) -> search cs s reverse model
        | None -> model

module Action =
    let private runAsync (f: unit -> 'a) = f |> Task.Run |> Async.AwaitTask

    let private performedAction action model =
        { model with
            UndoStack = action :: model.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete action model.PathFormat
        }

    let private setInputSelection cursorPos model =
        let fullLen = model.InputText.Length
        let nameLen = Path.SplitName model.InputText |> fst |> String.length
        { model with
            InputTextSelection =
                match cursorPos with
                | Begin -> (0, 0)
                | EndName -> (nameLen, 0)
                | End -> (fullLen, 0)
                | ReplaceName -> (0, nameLen)
                | ReplaceAll -> (0, fullLen)
        }

    let startInput (fsReader: IFileSystemReader) inputMode model = result {
        let! allowed =
            match inputMode with
            | Input CreateFile
            | Input CreateFolder ->
                match fsReader.GetNode model.Location with
                | Ok (Some node) when node.Type.CanCreateIn -> Ok true
                | Ok _ -> Error <| CannotPutHere
                | Error e -> Error <| ActionError ("create item", e)
            | Input (Rename _)
            | Confirm Delete -> Ok model.SelectedNode.Type.CanModify 
            | _ -> Ok true
        if allowed then
            let model = { model with InputMode = Some inputMode }
            match inputMode with
            | Input (Rename pos) ->
                return { model with InputText = model.SelectedNode.Name }
                       |> setInputSelection pos 
            | _ ->
                return { model with InputText = "" }
        else return model
    }

    let create (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) nodeType name model = asyncSeqResult {
        let createPath = model.Location.Join name
        let action = CreatedItem { Path = createPath; Name = name; Type = nodeType;
                                   Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }
        let! existing = fsReader.GetNode createPath |> itemActionError action model.PathFormat
        match existing with
        | None ->
            do! fsWriter.Create nodeType createPath |> itemActionError action model.PathFormat
            let! model = Nav.openPath fsReader model.Location (SelectName name) model
            yield model |> performedAction (CreatedItem model.SelectedNode)
        | Some existing ->
            yield! Nav.openPath fsReader model.Location (SelectName existing.Name) model
            return CannotUseNameAlreadyExists ("create", nodeType, name, existing.IsHidden)
    }

    let undoCreate (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) node model = asyncSeqResult {
        if fsReader.IsEmpty node.Path then
            yield { model with Status = Some <| MainStatus.undoingCreate node }
            let! res = runAsync (fun () -> fsWriter.Delete node.Path)
            do! res |> itemActionError (DeletedItem (node, true)) model.PathFormat
            if model.Location = node.Path.Parent then
                yield! Nav.refresh fsReader model
        else
            return CannotUndoNonEmptyCreated node
    }

    let rename (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) node newName model = result {
        if node.Type.CanModify then
            let action = RenamedItem (node, newName)
            let newPath = node.Path.Parent.Join newName
            let! existing =
                if String.equalsIgnoreCase node.Name newName then Ok None
                else fsReader.GetNode newPath |> itemActionError action model.PathFormat
            match existing with
            | None ->
                do! fsWriter.Move node.Path newPath |> itemActionError action model.PathFormat
                let! model = Nav.openPath fsReader model.Location (SelectName newName) model
                return model |> performedAction action
            | Some existingNode ->
                return! Error <| CannotUseNameAlreadyExists ("rename", node.Type, newName, existingNode.IsHidden)
        else return model
    }

    let undoRename (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) oldNode currentName model = result {
        let parentPath = oldNode.Path.Parent
        let currentPath = parentPath.Join currentName
        let node = { oldNode with Name = currentName; Path = currentPath }
        let action = RenamedItem (node, oldNode.Name)
        let! existing =
            if String.equalsIgnoreCase oldNode.Name currentName then Ok None
            else fsReader.GetNode oldNode.Path |> itemActionError action model.PathFormat
        match existing with
        | None ->
            do! fsWriter.Move currentPath oldNode.Path |> itemActionError action model.PathFormat
            return! Nav.openPath fsReader parentPath (SelectName oldNode.Name) model
        | Some existingNode ->
            return! Error <| CannotUseNameAlreadyExists ("rename", oldNode.Type, oldNode.Name, existingNode.IsHidden)
    }

    let registerItem action (model: MainModel) =
        if model.SelectedNode.Type.CanModify then
            { model with
                YankRegister = Some (model.SelectedNode, action)
                Status = None
            }
        else model

    let getCopyName name i =
        let (nameNoExt, ext) = Path.SplitName name
        let number = if i = 0 then "" else sprintf " %i" (i+1)
        sprintf "%s (copy%s)%s" nameNoExt number ext

    let putItem (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) overwrite node putAction model = asyncSeqResult {
        let sameFolder = node.Path.Parent = model.Location
        match! fsReader.GetNode model.Location |> actionError "put item" with
        | Some container when container.Type.CanCreateIn ->
            if putAction = Move && sameFolder then
                return CannotMoveToSameFolder
        | _ -> return CannotPutHere
        let! newName =
            match putAction with
            | Copy when sameFolder ->
                let unused name =
                    match fsReader.GetNode (model.Location.Join name) with
                    | Ok None -> true
                    | _ -> false
                Seq.init 99 (getCopyName node.Name)
                |> Seq.tryFind unused
                |> Result.ofOption (TooManyCopies node.Name)
            | Shortcut ->
                Ok (node.Name + ".lnk")
            | _ ->
                Ok node.Name
        let newPath = model.Location.Join newName
        let action = PutItem (putAction, node, newPath)
        let fileSysAction =
            match putAction with
            | Move -> fsWriter.Move
            | Copy -> fsWriter.Copy
            | Shortcut -> fsWriter.CreateShortcut
        let! existing = fsReader.GetNode newPath |> itemActionError action model.PathFormat
        match existing with
        | Some existing when not overwrite ->
            // refresh node list to make sure we can see the existing file
            let tempShowHidden = not model.ShowHidden && existing.IsHidden
            let refreshModel = { model with ShowHidden = if tempShowHidden then true else model.ShowHidden }
            let! model = Nav.openPath fsReader model.Location (SelectName existing.Name) refreshModel
            yield
                { model with
                    ShowHidden = if tempShowHidden then false else model.ShowHidden
                    InputMode = Some (Confirm (Overwrite (putAction, node, existing)))
                    InputText = ""
                }
        | _ ->
            yield { model with Status = MainStatus.runningAction action model.PathFormat }
            let! res = runAsync (fun () -> fileSysAction node.Path newPath)
            do! res |> itemActionError action model.PathFormat
            let! model = Nav.openPath fsReader model.Location (SelectName newName) model
            yield model |> performedAction action
    }

    let put fsReader fsWriter overwrite model = asyncSeqResult {
        match model.YankRegister with
        | None -> ()
        | Some (node, putAction) ->
            let! model = putItem fsReader fsWriter overwrite node putAction model
            if model.InputMode.IsNone then
                yield { model with YankRegister = None }
    }

    let undoMove (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) node currentPath (model: MainModel) = asyncSeqResult {
        let from = { node with Path = currentPath; Name = currentPath.Name }
        let action = PutItem (Move, from, node.Path)
        let! existing = fsReader.GetNode node.Path |> itemActionError action model.PathFormat
        match existing with
        | Some _ ->
            // TODO: prompt for overwrite here?
            return CannotUndoMoveToExisting node
        | None ->
            yield { model with Status = Some <| MainStatus.undoingMove node }
            let! res = runAsync (fun () -> fsWriter.Move currentPath node.Path)
            do! res |> itemActionError action model.PathFormat
            yield! Nav.openPath fsReader node.Path.Parent (SelectName node.Name) model
    }

    let undoCopy (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) node (currentPath: Path) (model: MainModel) = asyncSeqResult {
        let copyModified =
            match fsReader.GetNode currentPath with
            | Ok (Some copy) -> copy.Modified
            | _ -> None
        let isDeletionPermanent =
            match node.Modified, copyModified with
            | Some orig, Some copy when orig = copy -> true
            | _ -> false
        let action = DeletedItem ({ node with Path = currentPath }, isDeletionPermanent)
        let fileSysFunc = if isDeletionPermanent then fsWriter.Delete else fsWriter.Recycle
        yield { model with Status = Some <| MainStatus.undoingCopy node isDeletionPermanent }
        let! res = runAsync (fun () -> fileSysFunc currentPath)
        do! res |> itemActionError action model.PathFormat
        if model.Location = currentPath.Parent then
            yield! Nav.refresh fsReader model
    }

    let undoShortcut (fsReader: IFileSystemReader) (fsWriter: IFileSystemWriter) shortcutPath (model: MainModel) = result {
        let action = DeletedItem ({ Node.Empty with Path = shortcutPath; Name = shortcutPath.Name; Type = File }, true)
        do! fsWriter.Delete shortcutPath |> itemActionError action model.PathFormat
        if model.Location = shortcutPath.Parent then
            return! Nav.refresh fsReader model
        else
            return model
    }

    let clipCopy (os: IOperatingSystem) (model: MainModel) = result {
        let node = model.SelectedNode
        match node.Type with
        | File | Folder ->
            do! os.CopyToClipboard node.Path |> actionError "copy to clipboard"
        | _ -> ()
        return { model with Status = Some (MainStatus.clipboardCopy (node.Path.Format model.PathFormat)) }
    }

    let delete fsReader (fsWriter: IFileSystemWriter) node permanent (model: MainModel) = asyncSeqResult {
        if node.Type.CanModify then
            let action = DeletedItem (node, permanent)
            let fileSysFunc = if permanent then fsWriter.Delete else fsWriter.Recycle
            yield { model with Status = MainStatus.runningAction action model.PathFormat }
            let! res = runAsync (fun () -> fileSysFunc node.Path)
            do! res |> itemActionError action model.PathFormat
            yield! Nav.refresh fsReader model |> Result.map (performedAction action)
    }

    let recycle fsReader fsWriter (config: Config) (model: MainModel) = asyncSeqResult {
        if model.SelectedNode.Type = NetHost then
            let host = model.SelectedNode.Name
            config.RemoveNetHost host
            config.Save()
            let! model = Nav.refresh fsReader model
            yield { model with Status = Some <| MainStatus.removedNetworkHost host }
        else
            yield! delete fsReader fsWriter model.SelectedNode false model
    }

    let undo fsReader fsWriter model = asyncSeqResult {
        match model.UndoStack with
        | action :: rest ->
            let model = { model with UndoStack = rest }
            let! model =
                match action with
                | CreatedItem node ->
                    undoCreate fsReader fsWriter node model
                | RenamedItem (oldNode, curName) ->
                    undoRename fsReader fsWriter oldNode curName model
                    |> AsyncSeq.singleton
                | PutItem (Move, node, newPath) ->
                    undoMove fsReader fsWriter node newPath model
                | PutItem (Copy, node, newPath) ->
                    undoCopy fsReader fsWriter node newPath model
                | PutItem (Shortcut, node, newPath) ->
                    undoShortcut fsReader fsWriter newPath model
                    |> AsyncSeq.singleton
                | DeletedItem (node, permanent) ->
                    Error (CannotUndoDelete (permanent, node))
                    |> AsyncSeq.singleton
            yield
                { model with
                    RedoStack = action :: model.RedoStack
                    Status = Some <| MainStatus.undoAction action model.PathFormat
                }
        | [] -> return NoUndoActions
    }

    let redo fsReader fsWriter model = asyncSeqResult {
        match model.RedoStack with
        | action :: rest ->
            let model = { model with RedoStack = rest }
            let goToPath (nodePath: Path) =
                let path = nodePath.Parent
                if path <> model.Location then
                    Nav.openPath fsReader path SelectNone model
                else Ok model
            let! model = asyncSeqResult {
                match action with
                | CreatedItem node ->
                    let! model = goToPath node.Path
                    yield! create fsReader fsWriter node.Type node.Name model
                | RenamedItem (node, newName) ->
                    let! model = goToPath node.Path
                    yield! rename fsReader fsWriter node newName model
                | PutItem (putAction, node, newPath) ->
                    let! model = goToPath newPath
                    yield { model with Status = MainStatus.redoingAction action model.PathFormat }
                    yield! putItem fsReader fsWriter false node putAction model
                | DeletedItem (node, permanent) ->
                    let! model = goToPath node.Path
                    yield { model with Status = MainStatus.redoingAction action model.PathFormat }
                    yield! delete fsReader fsWriter node permanent model
            }
            yield
                { model with
                    RedoStack = rest
                    Status = Some <| MainStatus.redoAction action model.PathFormat
                }
        | [] -> return NoRedoActions
    }

    let openSplitScreenWindow (os: IOperatingSystem) getScreenBounds model = result {
        let mapFst f t = (fst t |> f, snd t)
        let fitRect = Rect.ofPairs model.WindowLocation (model.WindowSize |> mapFst ((*) 2))
                      |> Rect.fit (getScreenBounds())
        let model =
            { model with
                WindowLocation = fitRect.Location
                WindowSize = fitRect.Size |> mapFst (flip (/) 2)
            }

        let left, top = model.WindowLocation
        let width, height = model.WindowSize
        let path = (model.Location.Format Windows).TrimEnd([|'\\'|])
        let args = sprintf "\"%s\" --location=%i,%i --size=%i,%i"
                           path (left + width) top width height

        let! koffeePath = Path.Parse (System.Reflection.Assembly.GetExecutingAssembly().Location)
                          |> Result.ofOption CouldNotFindKoffeeExe
        let folder = koffeePath.Parent
        do! os.LaunchApp (koffeePath.Format Windows) folder args
            |> Result.mapError (fun e -> CouldNotOpenApp ("Koffee", e))
        return model
    }

    let openExplorer (os: IOperatingSystem) (model: MainModel) =
        os.OpenExplorer model.SelectedNode
        { model with Status = Some <| MainStatus.openExplorer }

    let openFileWith (os: IOperatingSystem) (model: MainModel) = result {
        let node = model.SelectedNode
        match node.Type with
        | File ->
            do! os.OpenFileWith node.Path |> actionError "open file with"
            return { model with Status = Some <| MainStatus.openFile node.Name }
        | _ ->
            return model
    }

    let openProperties (os: IOperatingSystem) (model: MainModel) = result {
        let node = model.SelectedNode
        match node.Type with
        | File | Folder ->
            do! os.OpenProperties node.Path |> actionError "open properties"
            return { model with Status = Some <| MainStatus.openProperties node.Name }
        | _ ->
            return model
    }

    let openCommandLine (os: IOperatingSystem) (config: Config) model = result {
        if model.Location <> Path.Root then
            do! os.LaunchApp config.CommandlinePath model.Location ""
                |> Result.mapError (fun e -> CouldNotOpenApp ("Commandline tool", e))
            return { model with Status = Some <| MainStatus.openCommandLine model.LocationFormatted }
        else return model
    }

    let openWithTextEditor (os: IOperatingSystem) (config: Config) (model: MainModel) = result {
        match model.SelectedNode.Type with
        | File ->
            let args = model.SelectedNode.Path.Format Windows |> sprintf "\"%s\""
            do! os.LaunchApp config.TextEditor model.Location args
                |> Result.mapError (fun e -> CouldNotOpenApp ("Text Editor", e))
            return { model with Status = Some <| MainStatus.openTextEditor model.SelectedNode.Name }
        | _ -> return model
    }

    let openSettings fsReader openSettings (config: Config) model = result {
        openSettings ()
        return!
            loadConfig fsReader config model
            |> Nav.refresh fsReader
    }

let initModel (config: Config) (fsReader: IFileSystemReader) startOptions model =
    let model =
        { loadConfig fsReader config model with
            WindowLocation =
                startOptions.StartLocation |> Option.defaultWith (fun () ->
                    let isFirstInstance =
                        System.Diagnostics.Process.GetProcesses()
                        |> Seq.where (fun p -> String.equalsIgnoreCase p.ProcessName "koffee")
                        |> Seq.length
                        |> (=) 1
                    if isFirstInstance then (config.Window.Left, config.Window.Top)
                    else (config.Window.Left + 30, config.Window.Top + 30))
            WindowSize = startOptions.StartSize |? (config.Window.Width, config.Window.Height)
            SaveWindowSettings = startOptions.StartLocation.IsNone && startOptions.StartSize.IsNone
        }
    let paths =
        (startOptions.StartPath |> Option.toList) @
            match config.StartPath with
            | RestorePrevious -> [ config.PreviousPath; config.DefaultPath ]
            | DefaultPath -> [ config.DefaultPath; config.PreviousPath ]
    let rec openPath error (paths: string list) =
        let withError (m: MainModel) = 
            match error with
            | Some e -> m.WithError e
            | None -> m
        match paths with
        | [] -> model |> withError
        | start :: paths ->
            let prevPath = paths |> Seq.choose Path.Parse |> Seq.tryHead |? Path.Root
            match Nav.openUserPath fsReader start { model with Location = prevPath } with
            | Ok model -> model |> withError
            | Error e -> openPath (Some (error |? e)) paths
    openPath None paths

let inputCharTyped fsReader fsWriter (config: Config) cancelInput char model = asyncSeqResult {
    let withBookmark char model =
        let winPath = model.Location.Format Windows
        config.SetBookmark char winPath
        config.Save()
        { model with Status = Some <| MainStatus.setBookmark char winPath }
    match model.InputMode with
    | Some (Input (Find _)) ->
        if char = ';' then // TODO: read key binding?
            cancelInput ()
            yield Cursor.findNext model
    | Some (Prompt mode) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match mode with
        | GoToBookmark ->
            match config.GetBookmark char with
            | Some path ->
                yield model
                yield! Nav.openUserPath fsReader path model
            | None ->
                yield { model with Status = Some <| MainStatus.noBookmark char }
        | SetBookmark ->
            match config.GetBookmark char |> Option.bind Path.Parse with
            | Some existingPath ->
                yield
                    { model with
                        InputMode = Some (Confirm (OverwriteBookmark (char, existingPath)))
                        InputText = ""
                    }
            | None -> 
                yield withBookmark char model
        | DeleteBookmark ->
            match config.GetBookmark char with
            | Some path ->
                config.RemoveBookmark char
                config.Save()
                yield { model with Status = Some <| MainStatus.deletedBookmark char path }
            | None ->
                yield { model with Status = Some <| MainStatus.noBookmark char }
    | Some (Confirm confirmType) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match char with
        | 'y' ->
            match confirmType with
            | Overwrite (action, src, _) ->
                let! model = Action.putItem fsReader fsWriter true src action model
                yield { model with YankRegister = None }
            | Delete ->
                yield! Action.delete fsReader fsWriter model.SelectedNode true model
            | OverwriteBookmark (char, _) ->
                yield withBookmark char model
        | 'n' ->
            let model = { model with Status = Some <| MainStatus.cancelled }
            match confirmType with
            | Overwrite _ when not model.ShowHidden && model.Nodes |> Seq.exists (fun n -> n.IsHidden) ->
                // if we were temporarily showing hidden files, refresh
                yield! Nav.refresh fsReader model
            | _ ->
                yield model
        | _ -> ()
    | _ -> ()
}

let inputChanged model =
    match model.InputMode with
    | Some (Input (Find _)) when String.isNotEmpty model.InputText ->
        Cursor.find model.InputText model
    | _ -> model

let inputDelete cancelInput model =
    match model.InputMode with
    | Some (Prompt GoToBookmark) | Some (Prompt SetBookmark) ->
        cancelInput ()
        { model with InputMode = Some (Prompt DeleteBookmark) }
    | _ -> model

let submitInput fsReader fsWriter os (config: Config) model = asyncSeqResult {
    match model.InputMode with
    | Some (Input (Find multi)) ->
        let model =
            if multi then { model with InputText = ""  }
            else { model with InputMode = None }
        yield model
        yield! Nav.openSelected fsReader os model
    | Some (Input Search) ->
        let model = { model with InputMode = None }
        let! search, caseSensitive = Cursor.parseSearch model.InputText
        let caseSensitive = caseSensitive |? config.SearchCaseSensitive
        yield Cursor.search caseSensitive search false model
    | Some (Input CreateFile) ->
        let model = { model with InputMode = None }
        yield model
        yield! Action.create fsReader fsWriter File model.InputText model
    | Some (Input CreateFolder) ->
        let model = { model with InputMode = None }
        yield model
        yield! Action.create fsReader fsWriter Folder model.InputText model
    | Some (Input (Rename _)) ->
        let model = { model with InputMode = None }
        yield model
        yield! Action.rename fsReader fsWriter model.SelectedNode model.InputText model
    | _ -> ()
}

let keyPress dispatcher keyBindings chord handleKey model = asyncSeq {
    let event, model =
        if chord = (ModifierKeys.None, Key.Escape) then
            handleKey ()
            if model.InputMode.IsSome then
                (None, { model with InputMode = None })
            else if not model.KeyCombo.IsEmpty then
                (None, { model with KeyCombo = [] })
            else
                (None, { model with Status = None })
        else
            let keyCombo = List.append model.KeyCombo [chord]
            match KeyBinding.getMatch keyBindings keyCombo with
            | KeyBinding.Match newEvent ->
                handleKey ()
                (Some newEvent, { model with KeyCombo = [] })
            | KeyBinding.PartialMatch ->
                handleKey ()
                (None, { model with KeyCombo = keyCombo })
            | KeyBinding.NoMatch ->
                (None, { model with KeyCombo = [] })
    match event with
    | Some e ->
        match dispatcher e with
        | Sync handler ->
            yield handler model
        | Async handler ->
            yield! handler model
    | None ->
        yield model
}

let windowLocationChanged (config: Config) (left, top) model =
    if model.SaveWindowSettings then
        config.Window.Left <- left
        config.Window.Top <- top
        config.Save()
    { model with WindowLocation = (left, top) }

let windowSizeChanged (config: Config) (width, height) model =
    if model.SaveWindowSettings then
        config.Window.Width <- width
        config.Window.Height <- height
        config.Save()
    { model with WindowSize = (width, height) }

let windowMaximized (config: Config) maximized model =
    if model.SaveWindowSettings then
        config.Window.IsMaximized <- maximized
        config.Save()
    model

let closed (config: Config) model =
    config.PreviousPath <- model.Location.Format Windows
    config.Save()
    model

let SyncResult handler =
    Sync (fun (model: MainModel) ->
        match handler model with
        | Ok m -> m
        | Error e -> model.WithError e
    )

let AsyncResult handler =
    Async (fun (model: MainModel) -> asyncSeq {
        let mutable last = model
        for r in handler model |> AsyncSeq.takeWhileInclusive Result.isOk do
            match r with
            | Ok m ->
                last <- m
                yield m
            | Error e ->
                yield last.WithError e
    })

let rec dispatcher fsReader fsWriter os getScreenBounds config keyBindings openSettings closeWindow evt =
    let dispatch =
        dispatcher fsReader fsWriter os getScreenBounds config keyBindings openSettings closeWindow
    let handler =
        match evt with
        | KeyPress (chord, handler) -> Async (keyPress dispatch keyBindings chord handler.Handle)
        | CursorUp -> Sync (fun m -> m.WithCursorRel -1)
        | CursorUpHalfPage -> Sync (fun m -> m.WithCursorRel -m.HalfPageSize)
        | CursorDown -> Sync (fun m -> m.WithCursorRel 1)
        | CursorDownHalfPage -> Sync (fun m -> m.WithCursorRel m.HalfPageSize)
        | CursorToFirst -> Sync (fun m -> m.WithCursor 0)
        | CursorToLast -> Sync (fun m -> m.WithCursor (m.Nodes.Length - 1))
        | OpenPath handler -> SyncResult (Nav.openInputPath fsReader handler)
        | OpenSelected -> SyncResult (Nav.openSelected fsReader os)
        | OpenParent -> SyncResult (Nav.openParent fsReader)
        | Back -> SyncResult (Nav.back fsReader)
        | Forward -> SyncResult (Nav.forward fsReader)
        | Refresh -> SyncResult (Nav.refresh fsReader)
        | Undo -> AsyncResult (Action.undo fsReader fsWriter)
        | Redo -> AsyncResult (Action.redo fsReader fsWriter)
        | StartPrompt promptType -> SyncResult (Action.startInput fsReader (Prompt promptType))
        | StartConfirm confirmType -> SyncResult (Action.startInput fsReader (Confirm confirmType))
        | StartInput inputType -> SyncResult (Action.startInput fsReader (Input inputType))
        | InputCharTyped (c, handler) -> AsyncResult (inputCharTyped fsReader fsWriter config handler.Handle c)
        | InputChanged -> Sync (inputChanged)
        | InputDelete handler -> Sync (inputDelete handler.Handle)
        | SubmitInput -> AsyncResult (submitInput fsReader fsWriter os config)
        | CancelInput -> Sync (fun m -> { m with InputMode = None })
        | FindNext -> Sync Cursor.findNext
        | SearchNext -> Sync (Cursor.searchNext false)
        | SearchPrevious -> Sync (Cursor.searchNext true)
        | StartAction action -> Sync (Action.registerItem action)
        | Put -> AsyncResult (Action.put fsReader fsWriter false)
        | ClipCopy -> SyncResult (Action.clipCopy os)
        | Recycle -> AsyncResult (Action.recycle fsReader fsWriter config)
        | SortList field -> SyncResult (Nav.sortList fsReader field)
        | ToggleHidden -> SyncResult (Nav.toggleHidden fsReader)
        | OpenSplitScreenWindow -> SyncResult (Action.openSplitScreenWindow os getScreenBounds)
        | OpenFileWith -> SyncResult (Action.openFileWith os)
        | OpenProperties -> SyncResult (Action.openProperties os)
        | OpenWithTextEditor -> SyncResult (Action.openWithTextEditor os config)
        | OpenExplorer -> Sync (Action.openExplorer os)
        | OpenCommandLine -> SyncResult (Action.openCommandLine os config)
        | OpenSettings -> SyncResult (Action.openSettings fsReader openSettings config)
        | Exit -> Sync (fun m -> closeWindow(); m)
        | ConfigChanged -> Sync (loadConfig fsReader config)
        | PageSizeChanged size -> Sync (fun m -> { m with PageSize = size })
        | WindowLocationChanged (l, t) -> Sync (windowLocationChanged config (l, t))
        | WindowSizeChanged (w, h) -> Sync (windowSizeChanged config (w, h))
        | WindowMaximizedChanged maximized -> Sync (windowMaximized config maximized)
        | Closed -> Sync (closed config)
    let isBusy model =
        match model.Status with
        | Some (Busy _) -> true
        | _ -> false
    match handler with
    | handler when evt = ConfigChanged -> handler
    | Sync handler ->
        Sync (fun model ->
            if not (isBusy model) then
                handler model
            else
                model
        )
    | Async handler ->
        Async (fun model -> asyncSeq {
            if not (isBusy model) then
                yield! handler model
        })


open Koffee.MainView

let start fsReader fsWriter os getScreenBounds (config: Config) keyBindings openSettings closeWindow startOptions view =
    let model = MainModel.Default |> initModel config fsReader startOptions
    let dispatch = dispatcher fsReader fsWriter os getScreenBounds config keyBindings openSettings closeWindow
    Framework.start (binder config) (events config) dispatch view model
