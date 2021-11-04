module Koffee.Main.Action

open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let private performedAction action (model: MainModel) =
    { model with
        UndoStack = action :: model.UndoStack
        RedoStack = []
    } |> fun m -> m.WithStatus (MainStatus.actionComplete action model.PathFormat)

let private setInputSelection cursorPos model =
    let fullLen = model.InputText.Length
    let nameLen =
        if model.SelectedItem.Type = File then
            Path.SplitName model.InputText |> fst |> String.length
        else
            fullLen
    { model with
        InputTextSelection =
            match cursorPos with
            | Begin -> (0, 0)
            | EndName -> (nameLen, 0)
            | End -> (fullLen, 0)
            | ReplaceName -> (0, nameLen)
            | ReplaceAll -> (0, fullLen)
    }

let startInput (fsReader: IFileSystemReader) inputMode (model: MainModel) = result {
    let inputMode =
        if inputMode = Prompt SetBookmark && model.SearchCurrent.IsSome then
            Prompt SetSavedSearch
        else
            inputMode
    let! allowed =
        match inputMode with
        | Input CreateFile
        | Input CreateFolder ->
            if model.IsSearchingSubFolders then
                Error CannotPutHere
            else
                match fsReader.GetItem model.Location with
                | Ok (Some item) when item.Type.CanCreateIn -> Ok true
                | Ok _ -> Error <| CannotPutHere
                | Error e -> Error <| ActionError ("create item", e)
        | Input (Rename _)
        | Confirm Delete -> Ok model.SelectedItem.Type.CanModify
        | _ -> Ok true
    if allowed then
        let model = { model with InputMode = Some inputMode }
        match inputMode with
        | Input Search ->
            let input = model.SearchCurrent |> Option.map (fun s -> s.Terms) |? ""
            return { model with InputText = input }
                   |> setInputSelection End
        | Input (Rename pos) ->
            return { model with InputText = model.SelectedItem.Name }
                   |> setInputSelection pos
        | _ ->
            return { model with InputText = "" }
    else
        return model
}

let create (fs: IFileSystem) itemType name model = asyncSeqResult {
    let createPath = model.Location.Join name
    let action = CreatedItem (Item.Basic createPath name itemType)
    let! existing = fs.GetItem createPath |> itemActionError action model.PathFormat
    match existing with
    | None ->
        do! fs.Create itemType createPath |> itemActionError action model.PathFormat
        let! model = Nav.openPath fs model.Location (SelectName name) model
        yield model |> performedAction (CreatedItem model.SelectedItem)
    | Some existing ->
        yield! Nav.openPath fs model.Location (SelectItem (existing, true)) model
        return CannotUseNameAlreadyExists ("create", itemType, name, existing.IsHidden)
}

let undoCreate (fs: IFileSystem) item (model: MainModel) = asyncSeqResult {
    if fs.IsEmpty item.Path then
        yield model.WithStatus (MainStatus.undoingCreate item)
        let! res = runAsync (fun () -> fs.Delete item.Path)
        do! res |> itemActionError (DeletedItem (item, true)) model.PathFormat
        if model.Location = item.Path.Parent then
            yield! Nav.refresh fs model
    else
        return CannotUndoNonEmptyCreated item
}

let rename (fs: IFileSystem) item newName (model: MainModel) = result {
    if item.Type.CanModify then
        let action = RenamedItem (item, newName)
        let newPath = item.Path.Parent.Join newName
        let! existing =
            if String.equalsIgnoreCase item.Name newName then Ok None
            else fs.GetItem newPath |> itemActionError action model.PathFormat
        match existing with
        | None ->
            do! fs.Move item.Path newPath |> itemActionError action model.PathFormat
            let newItem = { item with Name = newName; Path = newPath }
            let substitute = List.map (fun i -> if i = item then newItem else i)
            return
                { model with Directory = model.Directory |> substitute }
                |> (
                    if model.SearchCurrent.IsSome then
                        fun m -> { m with Items = m.Items |> substitute }
                    else
                        Nav.listDirectory (SelectName newName)
                )
                |> performedAction action
        | Some existingItem ->
            return! Error <| CannotUseNameAlreadyExists ("rename", item.Type, newName, existingItem.IsHidden)
    else return model
}

let undoRename (fs: IFileSystem) oldItem currentName (model: MainModel) = result {
    let parentPath = oldItem.Path.Parent
    let currentPath = parentPath.Join currentName
    let item = { oldItem with Name = currentName; Path = currentPath }
    let action = RenamedItem (item, oldItem.Name)
    let! existing =
        if String.equalsIgnoreCase oldItem.Name currentName then Ok None
        else fs.GetItem oldItem.Path |> itemActionError action model.PathFormat
    match existing with
    | None ->
        do! fs.Move currentPath oldItem.Path |> itemActionError action model.PathFormat
        return! Nav.openPath fs parentPath (SelectName oldItem.Name) model
    | Some existingItem ->
        return! Error <| CannotUseNameAlreadyExists ("rename", oldItem.Type, oldItem.Name, existingItem.IsHidden)
}

let registerItem action (model: MainModel) =
    if model.SelectedItem.Type.CanModify then
        let reg = Some (model.SelectedItem.Path, model.SelectedItem.Type, action)
        { model with
            Config = { model.Config with YankRegister = reg }
            Status = None
        }
    else model

let getCopyName name i =
    let (nameNoExt, ext) = Path.SplitName name
    let number = if i = 0 then "" else sprintf " %i" (i+1)
    sprintf "%s (copy%s)%s" nameNoExt number ext

let putItem (fs: IFileSystem) overwrite item putAction model = asyncSeqResult {
    let sameFolder = item.Path.Parent = model.Location
    match! fs.GetItem model.Location |> actionError "put item" with
    | Some container when container.Type.CanCreateIn ->
        if putAction = Move && sameFolder then
            return CannotMoveToSameFolder
    | _ -> return CannotPutHere
    let! newName =
        match putAction with
        | Copy when sameFolder ->
            let unused name =
                match fs.GetItem (model.Location.Join name) with
                | Ok None -> true
                | _ -> false
            Seq.init 99 (getCopyName item.Name)
            |> Seq.tryFind unused
            |> Result.ofOption (TooManyCopies item.Name)
        | Shortcut ->
            Ok (item.Name + ".lnk")
        | _ ->
            Ok item.Name
    let newPath = model.Location.Join newName
    let action = PutItem (putAction, item, newPath)
    let fileSysAction =
        match putAction with
        | Move -> fs.Move
        | Copy -> fs.Copy
        | Shortcut -> fs.CreateShortcut
    let! existing = fs.GetItem newPath |> itemActionError action model.PathFormat
    match existing with
    | Some existing when not overwrite ->
        // refresh item list to make sure we can see the existing file
        let! model = Nav.openPath fs model.Location (SelectItem (existing, true)) model
        yield
            { model with
                InputMode = Some (Confirm (Overwrite (putAction, item, existing)))
                InputText = ""
            }
    | _ ->
        yield model.WithStatusOption (MainStatus.runningAction action model.PathFormat)
        let! res = runAsync (fun () -> fileSysAction item.Path newPath)
        do! res |> itemActionError action model.PathFormat
        let! model = Nav.openPath fs model.Location (SelectName newName) model
        yield model |> performedAction action
}

let put (fs: IFileSystem) overwrite (model: MainModel) = asyncSeqResult {
    match model.Config.YankRegister with
    | None -> ()
    | Some (path, _, putAction) ->
        if model.IsSearchingSubFolders then
            return CannotPutHere
        match! fs.GetItem path |> actionError "read yank register item" with
        | Some item ->
            let! model = putItem fs overwrite item putAction model
            if model.InputMode.IsNone then
                yield { model with Config = { model.Config with YankRegister = None } }
        | None ->
            return YankRegisterItemMissing (path.Format model.PathFormat)
}

let undoMove (fs: IFileSystem) item currentPath (model: MainModel) = asyncSeqResult {
    let from = { item with Path = currentPath; Name = currentPath.Name }
    let action = PutItem (Move, from, item.Path)
    let! existing = fs.GetItem item.Path |> itemActionError action model.PathFormat
    match existing with
    | Some _ ->
        // TODO: prompt for overwrite here?
        return CannotUndoMoveToExisting item
    | None ->
        yield model.WithStatus (MainStatus.undoingMove item)
        let! res = runAsync (fun () -> fs.Move currentPath item.Path)
        do! res |> itemActionError action model.PathFormat
        yield! Nav.openPath fs item.Path.Parent (SelectName item.Name) model
}

let undoCopy (fs: IFileSystem) item (currentPath: Path) (model: MainModel) = asyncSeqResult {
    let copyModified =
        match fs.GetItem currentPath with
        | Ok (Some copy) -> copy.Modified
        | _ -> None
    let isDeletionPermanent =
        match item.Modified, copyModified with
        | Some orig, Some copy when orig = copy -> true
        | _ -> false
    let action = DeletedItem ({ item with Path = currentPath }, isDeletionPermanent)
    let fileSysFunc = if isDeletionPermanent then fs.Delete else fs.Recycle
    yield model.WithStatus (MainStatus.undoingCopy item isDeletionPermanent)
    let! res = runAsync (fun () -> fileSysFunc currentPath)
    do! res |> itemActionError action model.PathFormat
    if model.Location = currentPath.Parent then
        yield! Nav.refresh fs model
}

let undoShortcut (fs: IFileSystem) shortcutPath (model: MainModel) = result {
    let action = DeletedItem ({ Item.Empty with Path = shortcutPath; Name = shortcutPath.Name; Type = File }, true)
    do! fs.Delete shortcutPath |> itemActionError action model.PathFormat
    if model.Location = shortcutPath.Parent then
        return! Nav.refresh fs model
    else
        return model
}

let clipCopy (os: IOperatingSystem) (model: MainModel) = result {
    let item = model.SelectedItem
    if item.Type <> Empty then
        do! os.CopyToClipboard item.Path |> actionError "copy to clipboard"
        return model.WithStatus (MainStatus.clipboardCopy (item.Path.Format model.PathFormat))
    else
        return model
}

let delete (fsWriter: IFileSystemWriter) item permanent (model: MainModel) = asyncSeqResult {
    if item.Type.CanModify then
        let action = DeletedItem (item, permanent)
        let fileSysFunc = if permanent then fsWriter.Delete else fsWriter.Recycle
        yield model.WithStatusOption (MainStatus.runningAction action model.PathFormat)
        let! res = runAsync (fun () -> fileSysFunc item.Path)
        do! res |> itemActionError action model.PathFormat
        yield
            { model with
                Directory = model.Directory |> List.except [item]
                Items = model.Items |> List.except [item] |> model.ItemsIfEmpty
            }.WithCursor model.Cursor
            |> performedAction action
}

let recycle fsWriter (model: MainModel) = asyncSeqResult {
    if model.SelectedItem.Type = NetHost then
        let host = model.SelectedItem.Name
        yield
            { model with
                Directory = model.Directory |> List.except [model.SelectedItem]
                Items = model.Items |> List.except [model.SelectedItem] |> model.ItemsIfEmpty
                History = model.History.WithoutNetHost host
            }
            |> fun m -> m.WithStatus (MainStatus.removedNetworkHost host)
            |> fun m -> m.WithCursor model.Cursor
    else
        yield! delete fsWriter model.SelectedItem false model
}

let rec private undoIter iter fs model = asyncSeqResult {
    match model.UndoStack with
    | action :: rest ->
        let model = { model with UndoStack = rest }
        yield model
        let! model =
            match action with
            | CreatedItem item ->
                undoCreate fs item model
            | RenamedItem (oldItem, curName) ->
                undoRename fs oldItem curName model
                |> AsyncSeq.singleton
            | PutItem (Move, item, newPath) ->
                undoMove fs item newPath model
            | PutItem (Copy, item, newPath) ->
                undoCopy fs item newPath model
            | PutItem (Shortcut, item, newPath) ->
                undoShortcut fs newPath model
                |> AsyncSeq.singleton
            | DeletedItem (item, permanent) ->
                Error (CannotUndoDelete (permanent, item))
                |> AsyncSeq.singleton
        let model = { model with RedoStack = action :: model.RedoStack }
        if iter < model.RepeatCount then
            yield! undoIter (iter + 1) fs model
        else
            yield model.WithStatus (MainStatus.undoAction action model.PathFormat model.RepeatCount)
    | [] -> return NoUndoActions
}

let undo fs = undoIter 1 fs

let rec private redoIter iter fs model = asyncSeqResult {
    match model.RedoStack with
    | action :: rest ->
        let model = { model with RedoStack = rest }
        yield model
        let goToPath (itemPath: Path) =
            let path = itemPath.Parent
            if path <> model.Location then
                Nav.openPath fs path SelectNone model
            else Ok model
        let! model = asyncSeqResult {
            match action with
            | CreatedItem item ->
                let! model = goToPath item.Path
                yield! create fs item.Type item.Name model
            | RenamedItem (item, newName) ->
                let! model = goToPath item.Path
                yield! rename fs item newName model
            | PutItem (putAction, item, newPath) ->
                let! model = goToPath newPath
                yield model.WithStatusOption (MainStatus.redoingAction action model.PathFormat)
                yield! putItem fs false item putAction model
            | DeletedItem (item, permanent) ->
                let! model = goToPath item.Path
                yield model.WithStatusOption (MainStatus.redoingAction action model.PathFormat)
                yield! delete fs item permanent model
        }
        let model = { model with RedoStack = rest }
        if iter < model.RepeatCount then
            yield! redoIter (iter + 1) fs model
        else
            yield model.WithStatus (MainStatus.redoAction action model.PathFormat model.RepeatCount)
    | [] -> return NoRedoActions
}

let redo fs = redoIter 1 fs
