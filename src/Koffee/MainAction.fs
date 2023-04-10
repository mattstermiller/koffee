module Koffee.Main.Action

open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let private progressIncrementer (progress: Event<float option>) total =
    let incr = 1.0 / float total
    fun _ -> progress.Trigger (Some incr)

let private performedAction action (model: MainModel) =
    { model with
        UndoStack = action :: model.UndoStack |> List.truncate model.Config.Limits.Undo
        RedoStack = []
    } |> fun m -> m.WithMessage (MainStatus.ActionComplete (action, model.PathFormat))

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
                Error MainStatus.CannotPutHere
            else
                match fsReader.GetItem model.Location with
                | Ok (Some item) when item.Type.CanCreateIn -> Ok true
                | Ok _ -> Error <| MainStatus.CannotPutHere
                | Error e -> Error <| MainStatus.ActionError ("create item", e)
        | Input (Rename _)
        | Confirm Delete -> Ok model.SelectedItem.Type.CanModify
        | _ -> Ok true
    if allowed then
        let model = { model with InputMode = Some inputMode }
        match inputMode with
        | Input Search ->
            let input = model.SearchCurrent |> Option.map (fun s -> s.Terms) |? ""
            return { model with InputText = input }
                   |> setInputSelection ReplaceAll
        | Input (Rename pos) ->
            return { model with InputText = model.SelectedItem.Name }
                   |> setInputSelection pos
        | _ ->
            return { model with InputText = "" }
    else
        return model
}

let create (fs: IFileSystem) itemType name model = asyncSeqResult {
    let itemPath = model.Location.Join name
    let action = CreatedItem (Item.Basic itemPath name itemType)
    let! existing = fs.GetItem itemPath |> itemActionError action model.PathFormat
    match existing with
    | None ->
        do! fs.Create itemType itemPath |> itemActionError action model.PathFormat
        let model = model |> performedAction action
        yield model
        yield! Nav.openPath fs model.Location (SelectName name) model
    | Some existing ->
        yield! Nav.openPath fs model.Location (SelectItem (existing, true)) model
        return MainStatus.CannotUseNameAlreadyExists ("create", itemType, name, existing.IsHidden)
}

let undoCreate (fs: IFileSystem) item (model: MainModel) = asyncSeqResult {
    if fs.IsEmpty item.Path then
        yield model.WithBusy (MainStatus.UndoingCreate item)
        let! res = runAsync (fun () -> fs.Delete item.Type item.Path)
        do! res |> itemActionError (DeletedItem (item, true)) model.PathFormat
        if model.Location = item.Path.Parent then
            yield! Nav.refresh fs model
    else
        return MainStatus.CannotUndoNonEmptyCreated item
}

let rename (fs: IFileSystem) item newName (model: MainModel) = result {
    if item.Type.CanModify then
        let action = RenamedItem (item, newName)
        let newPath = item.Path.Parent.Join newName
        let! existing =
            if String.equalsIgnoreCase item.Name newName
            then Ok None
            else fs.GetItem newPath |> itemActionError action model.PathFormat
        match existing with
        | None ->
            do! fs.Move item.Type item.Path newPath |> itemActionError action model.PathFormat
            let newItem = { item with Name = newName; Path = newPath }
            let substitute = List.map (fun i -> if i = item then newItem else i)
            return
                { model with
                    Directory = model.Directory |> substitute
                    History = model.History.WithPathReplaced item.Path newPath
                }
                |> (fun model ->
                    if model.SearchCurrent.IsSome
                    then { model with Items = model.Items |> substitute }
                    else Nav.listDirectory (SelectName newName) model
                )
                |> performedAction action
        | Some existingItem ->
            return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", item.Type, newName, existingItem.IsHidden)
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
        do! fs.Move oldItem.Type currentPath oldItem.Path |> itemActionError action model.PathFormat
        return!
            { model with History = model.History.WithPathReplaced item.Path oldItem.Path }
            |> Nav.openPath fs parentPath (SelectName oldItem.Name)
    | Some existingItem ->
        return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", oldItem.Type, oldItem.Name, existingItem.IsHidden)
}

let registerItem putType (model: MainModel) =
    if model.SelectedItem.Type.CanModify then
        let reg = Some (model.SelectedItem.Path, model.SelectedItem.Type, putType)
        { model with
            Config = { model.Config with YankRegister = reg }
            Status = None
        }
    else model

let getCopyName name i =
    let (nameNoExt, ext) = Path.SplitName name
    let number = if i = 0 then "" else string (i+1)
    sprintf "%s copy%s%s" nameNoExt number ext

let rec private enumeratePutItems (fsReader: IFileSystemReader) copy checkExists (dest: Path) (item: Item) = asyncSeq {
    let destExists =
        if not checkExists then
            false
        else
            match fsReader.GetItem dest with
            | Ok (Some _) -> true
            | _ -> false
    let sameBase = item.Path.Base = dest.Base
    match item.Type with
    | Folder when copy || not sameBase || destExists ->
        let! itemsResult = runAsync (fun () -> fsReader.GetItems item.Path)
        match itemsResult with
        | Ok [] -> yield Ok { Item = item; Dest = dest; DestExists = destExists }
        | Ok items ->
            for item in items do
                let dest = dest.Join item.Name
                yield! enumeratePutItems fsReader copy destExists dest item
        | Error error -> yield Error (item, error)
    | Folder | File ->
        yield Ok { Item = item; Dest = dest; DestExists = destExists }
    | _ -> ()
}

let private performPutItems (fs: IFileSystem) progress (items: (PutType * PutItem) list) =
    let incrementProgress = progressIncrementer progress items.Length
    let fileSysAction putType itemType =
        match putType with
        | Move -> fs.Move itemType
        | Copy -> fs.Copy itemType
        | Shortcut -> fs.CreateShortcut
    let mutable foldersChecked = Set []
    let ensureFolderExists (path: Path) = result {
        if not (foldersChecked |> Set.contains path) then
            foldersChecked <- foldersChecked.Add path
            match! fs.GetItem path with
            | None -> return! fs.Create Folder path
            | Some _ -> ()
    }
    runAsync (fun () ->
        items
        |> List.map (fun (putType, putItem) ->
            ensureFolderExists putItem.Dest.Parent
            |> Result.bind (fun () ->
                fileSysAction putType putItem.Item.Type putItem.Item.Path putItem.Dest
                |> Result.map (fun () -> putItem)
            )
            |> Result.mapError (fun e -> (putItem.Item, e))
            |>! incrementProgress
        )
        |>! (fun _ -> progress.Trigger None)
    )

let rec private deleteEmptyFolders (fs: IFileSystem) path : Async<Result<unit, Path * exn>> = asyncResult {
    match fs.GetItems path with
    | Ok items ->
        let (folders, files) = items |> List.partition (fun item -> item.Type = Folder)
        for folder in folders do
            let! res = deleteEmptyFolders fs folder.Path
            do! res
        if files |> List.isEmpty then
            return! runAsync (fun () ->
                fs.Delete Folder path |> Result.mapError (fun ex -> (path, ex))
            )
    | Error ex ->
        return! Error (path, ex)
}

let private performPut (fs: IFileSystem) (progress: Event<_>) isUndo enumErrors putType intent items (model: MainModel) = asyncSeqResult {
    let getItemAction putItem = if isUndo && putItem.DestExists then Copy else putType
    let! results =
        items
        |> List.map (fun item -> (getItemAction item, item))
        |> performPutItems fs progress
    let succeeded, putErrors = results |> Result.partition
    let action = PutItems (putType, intent, items)
    let openDest model = Nav.openPath fs intent.Dest.Parent (SelectName intent.Dest.Name) model
    match enumErrors @ putErrors with
    | [] ->
        let model =
            if not isUndo then
                model |> performedAction action
            else
                model
        let! deleteRes = asyncResult {
            // if folder was enumerated and its contents were moved, delete empty source folders
            if putType = Move && intent.Item.Type = Folder
                && not (items |> List.exists (fun i -> i.Item = intent.Item))
            then
                let! res = deleteEmptyFolders fs intent.Item.Path
                return! res |> Result.mapError (fun (path, ex) -> MainStatus.CouldNotDeleteMoveSource (path.Name, ex))
        }
        let model =
            if putType = Move then
                { model with History = model.History.WithPathReplaced intent.Item.Path intent.Dest }
            else
                model
        yield! model.WithResult deleteRes |> openDest
    | errorItems when succeeded |> List.isEmpty ->
        // if nothing succeeded, return error
        return MainStatus.PutError (isUndo, putType, errorItems, items.Length + enumErrors.Length)
    | errorItems ->
        // if partial success, update undo stack with successes
        // set error message instead of returning Error so that the caller flow is not short-circuited
        let model =
            if putType = Move then
                let pathReplacements = succeeded |> List.map (fun putItem -> putItem.Item.Path, putItem.Dest) |> Map
                { model with History = model.History.WithPathsReplaced pathReplacements }
            else
                model
        let model = openDest model |> Result.toOption |? model
        let error = MainStatus.PutError (isUndo, putType, errorItems, items.Length + enumErrors.Length)
        if isUndo then
            let undoIntent = { (intent |> PutItem.reverse) with DestExists = true }
            let action = PutItems (putType, undoIntent, succeeded |> List.map PutItem.reverse)
            yield
                { model.WithError error with
                    RedoStack = action :: model.RedoStack
                }
        else
            let action = PutItems (putType, intent, succeeded)
            yield
                { model.WithError error with
                    UndoStack = action :: model.UndoStack |> List.truncate model.Config.Limits.Undo
                    RedoStack = []
                }
}

let putItem (fs: IFileSystem) (progress: Event<float option>) overwrite item putType model = asyncSeqResult {
    let sameFolder = item.Path.Parent = model.Location
    match! fs.GetItem model.Location |> actionError "put item" with
    | Some container when container.Type.CanCreateIn ->
        if putType = Move && sameFolder then
            return MainStatus.CannotMoveToSameFolder
    | _ ->
        return MainStatus.CannotPutHere
    let! newName =
        match putType with
        | Copy when sameFolder ->
            let unused name =
                match fs.GetItem (model.Location.Join name) with
                | Ok None -> true
                | _ -> false
            Seq.init 99 (getCopyName item.Name)
            |> Seq.tryFind unused
            |> Result.ofOption (MainStatus.TooManyCopies item.Name)
        | Shortcut ->
            Ok (item.Name + ".lnk")
        | _ ->
            Ok item.Name
    let newPath = model.Location.Join newName
    let! existing = fs.GetItem newPath |> actionError "check destination path"
    match existing with
    | Some existing when not overwrite ->
        // refresh item list to make sure we can see the existing file
        let! model = Nav.openPath fs model.Location (SelectItem (existing, true)) model
        yield
            { model with
                InputMode = Some (Confirm (Overwrite (putType, item, existing)))
                InputText = ""
            }
    | _ ->
        yield model.WithBusy (MainStatus.PreparingPut (putType, item.Name))
        progress.Trigger (Some 0.0)
        let! enumerated =
            match putType with
            | Shortcut -> async { return [Ok { Item = item; Dest = newPath; DestExists = existing.IsSome }] }
            | Move | Copy -> enumeratePutItems fs (putType = Copy) true newPath item |> AsyncSeq.toListAsync
        let items, enumErrors = enumerated |> Result.partition
        let putItem = { Item = item; Dest = newPath; DestExists = existing.IsSome }
        match putType with
        | Move ->
            yield model.WithBusy (MainStatus.MovingItem (putItem, model.PathFormat))
        | Copy ->
            yield model.WithBusy (MainStatus.CopyingItem (putItem, model.PathFormat))
        | _ -> ()
        yield! performPut fs progress false enumErrors putType putItem items model
}

let put (fs: IFileSystem) progress overwrite (model: MainModel) = asyncSeqResult {
    match model.Config.YankRegister with
    | None -> ()
    | Some (path, _, putType) ->
        if model.IsSearchingSubFolders then
            return MainStatus.CannotPutHere
        match! fs.GetItem path |> actionError "read yank register item" with
        | Some item ->
            let! model = putItem fs progress overwrite item putType model
            if model.InputMode.IsNone then
                yield { model with Config = { model.Config with YankRegister = None } }
        | None ->
            return MainStatus.YankRegisterItemMissing (path.Format model.PathFormat)
}

let undoMove (fs: IFileSystem) progress (intent: PutItem) (moved: PutItem list) (model: MainModel) = asyncSeqResult {
    yield model.WithBusy (MainStatus.UndoingMove intent.Item)
    let items, existErrors =
        moved
        |> List.map PutItem.reverse
        |> List.map (fun putItem ->
            match fs.GetItem putItem.Dest with
            | Ok None -> Ok putItem
            | Ok (Some _) -> Error (putItem.Item, UndoMoveBlockedByExistingItemException() :> exn)
            | Error e -> Error (putItem.Item, e)
        )
        |> Result.partition
    let putItem = intent |> PutItem.reverse
    yield! performPut fs progress true existErrors Move putItem items model
}

let private performUndoCopy (fs: IFileSystem) progress (items: PutItem list) =
    let incrementProgress = progressIncrementer progress items.Length
    let shouldDelete putItem =
        let copyModified =
            match fs.GetItem putItem.Dest with
            | Ok (Some copy) -> copy.Modified
            | _ -> None
        match putItem.Item.Modified, copyModified with
        | Some orig, Some copy when orig = copy -> true
        | _ -> false
    runAsync (fun () ->
        items
        |> List.map (fun putItem ->
            (
                if putItem.DestExists then
                    Ok () // don't remove items that existed before the copy
                else if shouldDelete putItem then
                    fs.Delete putItem.Item.Type putItem.Dest
                else
                    fs.Recycle putItem.Item.Type putItem.Dest
            )
            |> Result.map (cnst putItem)
            |> Result.mapError (fun e -> ({ putItem.Item with Path = putItem.Dest }, e))
            |>! incrementProgress
        )
        |>! (fun _ -> progress.Trigger None)
    )

let undoCopy fs progress (intent: PutItem) copied (model: MainModel) = asyncSeqResult {
    yield model.WithBusy (MainStatus.UndoingCopy intent.Item)
    let! results = performUndoCopy fs progress copied
    match results |> Result.partition with
    | _, [] ->
        let! deleteRes = asyncResult {
            // delete empty copied folders
            if intent.Item.Type = Folder then
                let! res = deleteEmptyFolders fs intent.Dest
                return! res |> Result.mapError (fun (path, ex) -> MainStatus.CouldNotDeleteCopyDest (path.Name, ex))
        }
        let model = model.WithResult deleteRes
        if model.Location = intent.Dest.Parent then
            yield! Nav.refresh fs model
        else
            yield model
    | [], errors ->
        return MainStatus.PutError (true, Copy, errors, copied.Length)
    | succeeded, errors ->
        // if partial success, update undo stack with successes
        // set error message instead of returning Error so that the caller flow is not short-circuited
        let action = PutItems (Copy, intent, succeeded)
        let model =
            { model.WithError (MainStatus.PutError (true, Copy, errors, copied.Length)) with
                RedoStack = action :: model.RedoStack
            }
        if model.Location = intent.Dest.Parent then
            yield Nav.refresh fs model |> Result.toOption |? model
        else
            yield model
}

let undoShortcut (fs: IFileSystem) shortcutPath (model: MainModel) = result {
    let action = DeletedItem ({ Item.Empty with Path = shortcutPath; Name = shortcutPath.Name; Type = File }, true)
    do! fs.Delete File shortcutPath |> itemActionError action model.PathFormat
    if model.Location = shortcutPath.Parent then
        return! Nav.refresh fs model
    else
        return model
}

let clipCopy (os: IOperatingSystem) (model: MainModel) = result {
    let item = model.SelectedItem
    if item.Type <> Empty then
        do! os.CopyToClipboard item.Path |> actionError "copy to clipboard"
        return model.WithMessage (MainStatus.ClipboardCopy (item.Path.Format model.PathFormat))
    else
        return model
}

let rec private enumerateDeleteItems (fsReader: IFileSystemReader) (items: Item seq) = asyncSeq {
    for item in items do
        match item.Type with
        | Folder ->
            let! subItems = runAsync (fun () -> fsReader.GetItems item.Path)
            match subItems with
            | Ok subItems when not subItems.IsEmpty ->
                yield! enumerateDeleteItems fsReader subItems
                yield item
            | _ ->
                yield item
        | File ->
            yield item
        | _ -> ()
}

let delete (fs: IFileSystem) (progress: Event<float option>) item permanent (model: MainModel) = asyncSeqResult {
    if item.Type.CanModify then
        let action = DeletedItem (item, permanent)
        if permanent then
            yield model.WithBusy (MainStatus.PreparingDelete item.Name)
            progress.Trigger (Some 0.0)
            let! items = enumerateDeleteItems fs [item] |> AsyncSeq.toListAsync
            let incrementProgress = progressIncrementer progress items.Length
            yield model.WithBusy (MainStatus.DeletingItem (item, permanent))
            let! res = runAsync (fun () ->
                items
                |> Seq.map (fun i ->
                    fs.Delete i.Type i.Path
                    |> Result.mapError(fun ex -> (i, ex))
                    |>! incrementProgress
                )
                |> Result.accumulate
                |> Result.map ignore
            )
            progress.Trigger None
            do! res |> Result.mapError (fun errors -> MainStatus.DeleteError (errors, items.Length))
        else
            yield model.WithBusy (MainStatus.DeletingItem (item, permanent))
            let! res = runAsync (fun () -> fs.Recycle item.Type item.Path)
            do! res |> itemActionError action model.PathFormat
        yield
            { model with
                Directory = model.Directory |> List.except [item]
                Items = model.Items |> List.except [item] |> model.ItemsOrEmpty
                History = model.History.WithPathRemoved item.Path
            }.WithCursor model.Cursor
            |> performedAction action
}

let recycle fsWriter progress (model: MainModel) = asyncSeqResult {
    if model.SelectedItem.Type = NetHost then
        let host = model.SelectedItem.Name
        yield
            { model with
                Directory = model.Directory |> List.except [model.SelectedItem]
                Items = model.Items |> List.except [model.SelectedItem] |> model.ItemsOrEmpty
                History = model.History.WithoutNetHost host
            }
            |> fun m -> m.WithMessage (MainStatus.RemovedNetworkHost host)
            |> fun m -> m.WithCursor model.Cursor
    else
        yield! delete fsWriter progress model.SelectedItem false model
}

let rec private undoIter iter fs progress model = asyncSeqResult {
    match model.UndoStack with
    | action :: rest ->
        let oldRedoHead = model.RedoStack |> List.tryHead
        let model = { model.ClearStatus () with UndoStack = rest }
        yield model
        let! model =
            match action with
            | CreatedItem item ->
                undoCreate fs item model
            | RenamedItem (oldItem, curName) ->
                undoRename fs oldItem curName model
                |> AsyncSeq.singleton
            | PutItems (Move, intent, actual) ->
                undoMove fs progress intent actual model
            | PutItems (Copy, intent, actual) ->
                undoCopy fs progress intent actual model
            | PutItems (Shortcut, intent, _) ->
                undoShortcut fs intent.Dest model
                |> AsyncSeq.singleton
            | DeletedItem (item, permanent) ->
                Error (MainStatus.CannotUndoDelete (permanent, item))
                |> AsyncSeq.singleton
        let model =
            if model.RedoStack |> List.tryHead = oldRedoHead then
                { model with RedoStack = action :: model.RedoStack }
            else
                model
        if iter < model.RepeatCount then
            yield! undoIter (iter + 1) fs progress model
        else if not model.IsStatusError then
            yield model.WithMessage (MainStatus.UndoAction (action, model.PathFormat, model.RepeatCount))
        else
            yield model
    | [] -> return MainStatus.NoUndoActions
}

let undo fs = undoIter 1 fs

let rec private redoIter iter fs progress model = asyncSeqResult {
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
            | PutItems (putType, intent, _) ->
                let! model = goToPath intent.Dest
                match! fs.GetItem intent.Dest |> actionError "check destination path" with
                | Some existing when not intent.DestExists ->
                    yield Nav.select (SelectItem (existing, true)) model
                    return MainStatus.CannotRedoPutToExisting (putType, intent.Item, intent.Dest.Format model.PathFormat)
                | _ ->
                    match putType with
                    | Move ->
                        yield model.WithBusy (MainStatus.RedoingMovingItem (intent, model.PathFormat))
                    | Copy ->
                        yield model.WithBusy (MainStatus.RedoingCopyingItem (intent, model.PathFormat))
                    | _ -> ()
                    yield! putItem fs progress intent.DestExists intent.Item putType model
            | DeletedItem (item, permanent) ->
                let! model = goToPath item.Path
                yield model.WithBusy (MainStatus.RedoingDeletingItem (item, permanent))
                yield! delete fs progress item permanent model
        }
        let model = { model with RedoStack = rest }
        if iter < model.RepeatCount then
            yield! redoIter (iter + 1) fs progress model
        else
            let redoneAction = model.UndoStack |> List.tryHead |? action
            yield model.WithMessage (MainStatus.RedoAction (redoneAction, model.PathFormat, model.RepeatCount))
    | [] -> return MainStatus.NoRedoActions
}

let redo fs = redoIter 1 fs
