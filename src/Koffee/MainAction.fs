module Koffee.Main.Action

open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let private progressIncrementer (progress: Event<float option>) total =
    let incr = 1.0 / float total
    fun _ -> progress.Trigger (Some incr)

let private performedAction action (model: MainModel) =
    { model.WithMessage(MainStatus.ActionComplete (action, model.PathFormat))
           .WithPushedUndo(action) with
        RedoStack = []
    }

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
    if not (fs.IsEmpty item.Path) then
        return MainStatus.CannotUndoNonEmptyCreated item
    yield model.WithBusy (MainStatus.UndoingCreate item)
    let! res = runAsync (fun () -> fs.Delete item.Type item.Path)
    do! res |> itemActionError (DeletedItem (item, true)) model.PathFormat
    let model = { model with History = model.History.WithPathRemoved item.Path }
    if model.Location = item.Path.Parent then
        yield! Nav.refresh fs model
    else
        yield model
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

let rec private enumeratePutItems (fsReader: IFileSystemReader) (cancelToken: CancelToken) copy (dest: Path) (item: Item) =
    let rec iter checkExists dest item = asyncSeq {
        if not cancelToken.IsCancelled then
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
                | Ok [] ->
                    yield Ok { Item = item; Dest = dest; DestExists = destExists }
                | Ok items ->
                    for item in items do
                        let dest = dest.Join item.Name
                        yield! iter destExists dest item
                | Error error ->
                    yield Error (item, error)
            | Folder | File ->
                yield Ok { Item = item; Dest = dest; DestExists = destExists }
            | _ -> ()
    }
    iter true dest item

let private performPutItems (fs: IFileSystem) progress (cancelToken: CancelToken) isUndo putType (items: PutItem list) =
    let incrementProgress = progressIncrementer progress items.Length
    let fileSysAction putItem =
        // when undoing a move that was an overwrite, copy it back since a version of the item was there before
        let putType = if isUndo && putItem.DestExists then Copy else putType
        let itemType, src, dest = putItem.Item.Type, putItem.Item.Path, putItem.Dest
        if not isUndo && itemType = Folder && putItem.DestExists then
            Ok () // skip if folder already exists
        else
            match putType with
            | Move -> fs.Move itemType src dest
            | Copy when itemType = Folder -> fs.Create itemType dest
            | Copy -> fs.Copy itemType src dest
            | Shortcut -> fs.CreateShortcut src dest
    let mutable foldersChecked = Set []
    let ensureFolderExists destExists (path: Path) = result {
        if not (foldersChecked |> Set.contains path) then
            foldersChecked <- foldersChecked.Add path
            if not destExists || isUndo then
                match! fs.GetItem path with
                | None -> return! fs.Create Folder path
                | Some _ -> ()
    }
    runAsync (fun () ->
        items
        |> Seq.takeWhile (fun _ -> not cancelToken.IsCancelled)
        |> Seq.map (fun putItem ->
            ensureFolderExists putItem.DestExists putItem.Dest.Parent
            |> Result.bind (fun () -> fileSysAction putItem)
            |> Result.map (fun () -> putItem)
            |> Result.mapError (fun e -> (putItem.Item, e))
            |>! incrementProgress
        )
        |> Seq.toList
        |>! (fun _ -> progress.Trigger None)
    )

let private deleteEmptyFolders (fs: IFileSystem) path =
    let rec iter path = seq {
        match fs.GetItems path with
        | Ok items ->
            let (folders, files) = items |> List.partition (fun item -> item.Type = Folder)
            yield! folders |> Seq.collect (fun folder -> iter folder.Path)
            if files |> List.isEmpty then
                yield
                    fs.Delete Folder path
                    |> Result.map (cnst path)
                    |> Result.mapError (fun ex -> (path, ex))
        | Error ex ->
            yield Error (path, ex)
    }
    runAsync (fun () -> iter path |> Result.partition)

let private performPut (fs: IFileSystem) (progress: Event<_>) isUndo enumErrors putType intent items (model: MainModel) = asyncSeqResult {
    let! results = items |> performPutItems fs progress model.CancelToken isUndo putType
    let succeeded, putErrors = results |> Result.partition
    let errorItems = enumErrors @ putErrors

    // if nothing succeeded, return error
    if succeeded |> List.isEmpty then
        return MainStatus.PutError (isUndo, putType, errorItems, items.Length + enumErrors.Length)

    let! deletedFolders, deleteFolderErrors = async {
        // if folder was enumerated and its contents were moved, delete empty source folders
        if putType = Move && intent.Item.Type = Folder
            && not (items |> List.exists (fun i -> i.Item = intent.Item))
        then
            return! deleteEmptyFolders fs intent.Item.Path
        else
            return ([], [])
    }

    let getReversedAction intent actual cancelled =
        PutItems (putType, intent |> PutItem.reverse, actual |> List.map PutItem.reverse, cancelled)
    let cancelledAction reverse =
        if model.CancelToken.IsCancelled then
            let unsuccessful = items |> List.except succeeded
            if reverse
            then Some (getReversedAction intent unsuccessful true)
            else Some (PutItems (putType, intent, unsuccessful, true))
        else
            None
    let action = PutItems (putType, intent, succeeded, model.CancelToken.IsCancelled)

    let updateUndoRedo (model: MainModel) =
        if isUndo then
            // if there were errors, destination folder still exists so set DestExists to allow redo to merge
            let redoIntent = if not errorItems.IsEmpty then { intent with DestExists = true } else intent
            model
            |> Option.foldBack (fun action model -> model.WithPushedUndo action) (cancelledAction true)
            |> fun model -> model.WithPushedRedo (getReversedAction redoIntent succeeded model.CancelToken.IsCancelled)
        else
            model.WithPushedUndo action
            |> fun model -> { model with RedoStack = cancelledAction false |> Option.toList }
    let updateHistory model =
        if putType = Move then
            let shouldReplaceHistory putItem = not (isUndo && putItem.DestExists)
            let isCompleted = errorItems.IsEmpty && not model.CancelToken.IsCancelled
            let pathReplacements =
                if isCompleted && shouldReplaceHistory intent then
                    Map [intent.Item.Path, intent.Dest]
                else
                    succeeded
                    |> List.filter shouldReplaceHistory
                    |> List.map (fun putItem -> putItem.Item.Path, putItem.Dest)
                    |> Map
            { model with History = model.History.WithPathsReplaced(pathReplacements).WithPathsRemoved(deletedFolders) }
        else
            model
    let openDest model =
        model
        |> Nav.openPath fs intent.Dest.Parent (SelectName intent.Dest.Name)
        |> Result.toOption |? model // ignore error opening destination
    let status =
        option {
            // for partial success, set error message instead of returning Error so the caller flow is not short-circuited
            if not errorItems.IsEmpty then
                return MainStatus.Error (MainStatus.PutError (isUndo, putType, errorItems, items.Length + enumErrors.Length))
            if model.CancelToken.IsCancelled then
                return MainStatus.Message (MainStatus.CancelledPut (putType, isUndo, succeeded.Length, items.Length))
            return!
                deleteFolderErrors
                |> List.tryHead
                |> Option.map (fun (path, ex) -> MainStatus.Error (MainStatus.CouldNotDeleteMoveSource (path.Name, ex)))
            if not isUndo then
                return MainStatus.Message (MainStatus.ActionComplete (action, model.PathFormat))
        }
    yield
        model
        |> updateUndoRedo
        |> updateHistory
        |> Option.foldBack (fun s model -> model.WithStatus s) status
        |> openDest
}

let putToDestination (fs: IFileSystem) (progress: Event<float option>) isRedo overwrite putType item destPath model = asyncSeqResult {
    let! existing = fs.GetItem destPath |> actionError "check destination path"
    match existing with
    | Some existing when not overwrite && not isRedo ->
        // refresh item list to make sure we can see the existing file
        let! model = Nav.openPath fs model.Location (SelectItem (existing, true)) model
        yield
            { model with
                InputMode = Some (Confirm (Overwrite (putType, item, existing)))
                InputText = ""
            }
    | _ ->
        let model = { model with CancelToken = CancelToken() }
        let putItem = { Item = item; Dest = destPath; DestExists = existing.IsSome }
        yield model.WithBusy (MainStatus.PreparingPut (putType, item.Name))
        progress.Trigger (Some 0.0)
        let! enumerated =
            match putType with
            | Shortcut ->
                async { return [Ok putItem] }
            | Move | Copy ->
                enumeratePutItems fs model.CancelToken (putType = Copy) destPath item |> AsyncSeq.toListAsync
        if model.CancelToken.IsCancelled then
            yield model.WithMessage (MainStatus.CancelledPut (putType, false, 0, enumerated.Length))
        else
            let itemsToSkip =
                if isRedo && putType = Copy then
                    // if resuming cancelled copy, skip items already copied
                    model.UndoStack |> List.tryHead |> Option.bind (function
                        | PutItems (Copy, intent, actual, true) when PutItem.intentEquals intent putItem ->
                            Some (actual |> List.map (fun putItem -> putItem.Item))
                        | _ -> None
                    )
                else
                    None
            let filterItemsToSkip =
                match itemsToSkip with
                | Some itemsToSkip ->
                    List.filter (fun res ->
                        let item = res |> Result.toOption |> Option.map (fun putItem -> putItem.Item)
                        not (item |> Option.exists (Seq.containedIn itemsToSkip))
                    )
                | None -> id
            let errorOnExisting putResult =
                putResult |> Result.bind (fun putItem ->
                    if putItem.DestExists
                    then Error (putItem.Item, RedoPutBlockedByExistingItemException() :> exn)
                    else Ok putItem
                )
            let items, enumErrors =
                enumerated
                |> filterItemsToSkip
                |> if isRedo && not overwrite then List.map errorOnExisting else id
                |> Result.partition
            if putType = Move || putType = Copy then
                yield model.WithBusy (MainStatus.PuttingItem ((putType = Copy), isRedo, putItem, model.PathFormat))
            yield! performPut fs progress false enumErrors putType putItem items model
}

let putInLocation (fs: IFileSystem) progress isRedo overwrite putType item model = asyncSeqResult {
    let sameFolder = item.Path.Parent = model.Location
    if putType = Move && sameFolder then
        return MainStatus.CannotMoveToSameFolder
    do! fs.GetItem model.Location
        |> actionError "read put location"
        |> Result.okIf (Option.exists (fun l -> l.Type.CanCreateIn)) MainStatus.CannotPutHere
        |> Result.map ignore
    let! destName =
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
    let destPath = model.Location.Join destName
    yield! putToDestination fs progress isRedo overwrite putType item destPath model
}

let put (fs: IFileSystem) progress overwrite (model: MainModel) = asyncSeqResult {
    match model.Config.YankRegister with
    | None -> ()
    | Some (path, _, putType) ->
        if model.IsSearchingSubFolders then
            return MainStatus.CannotPutHere
        match! fs.GetItem path |> actionError "read yank register item" with
        | Some item ->
            let! model = putInLocation fs progress false overwrite putType item model
            let wasImmediatelyCanceled =
                match model.Status with Some (MainStatus.Message (MainStatus.CancelledPut (_, _, 0, _))) -> true | _ -> false
            // if not cancelled or opened input for confirmation, clear yank register
            if not wasImmediatelyCanceled && model.InputMode.IsNone then
                yield { model with Config = { model.Config with YankRegister = None } }
        | None ->
            return MainStatus.YankRegisterItemMissing (path.Format model.PathFormat)
}

let undoMove (fs: IFileSystem) progress (intent: PutItem) (moved: PutItem list) (model: MainModel) = asyncSeqResult {
    let model = { model with CancelToken = CancelToken() }
    yield model.WithBusy (MainStatus.UndoingPut (false, intent.Item))
    let! items, existErrors = runAsync (fun () ->
        moved
        |> Seq.takeWhile (fun _ -> not model.CancelToken.IsCancelled)
        |> Seq.map PutItem.reverse
        |> Seq.map (fun putItem ->
            match fs.GetItem putItem.Dest with
            | Ok None -> Ok putItem
            | Ok (Some _) -> Error (putItem.Item, UndoMoveBlockedByExistingItemException() :> exn)
            | Error e -> Error (putItem.Item, e)
        )
        |> Result.partition
    )
    if not model.CancelToken.IsCancelled then
        let putItem = intent |> PutItem.reverse
        yield! performPut fs progress true existErrors Move putItem items model
}

let private performUndoCopy (fs: IFileSystem) progress (cancelToken: CancelToken) (items: PutItem list) =
    let incrementProgress = progressIncrementer progress items.Length
    let shouldDelete putItem =
        let copyModified = fs.GetItem putItem.Dest |> Result.map (Option.bind (fun copiedItem -> copiedItem.Modified))
        match putItem.Item.Modified, copyModified with
        | origTime, Ok copyTime when origTime = copyTime -> true
        | _ -> false
    runAsync (fun () ->
        items
        |> Seq.takeWhile (fun _ -> not cancelToken.IsCancelled)
        |> Seq.map (fun putItem ->
            (
                if shouldDelete putItem
                then fs.Delete putItem.Item.Type putItem.Dest
                else fs.Recycle (putItem.Item.Size |? 0L) putItem.Item.Type putItem.Dest
            )
            |> Result.map (cnst putItem)
            |> Result.mapError (fun e -> ({ putItem.Item with Path = putItem.Dest }, e))
            |>! incrementProgress
        )
        |> Seq.toList
        |>! (fun _ -> progress.Trigger None)
    )

let undoCopy fs progress (intent: PutItem) copied (model: MainModel) = asyncSeqResult {
    let model = { model with CancelToken = CancelToken() }
    yield model.WithBusy (MainStatus.UndoingPut (true, intent.Item))
    let! results = performUndoCopy fs progress model.CancelToken copied
    let succeeded, errors = results |> Result.partition

    if not errors.IsEmpty && succeeded.IsEmpty then
        return MainStatus.PutError (true, Copy, errors, copied.Length)

    let! deletedFolders, deleteFolderErrors = async {
        // delete empty copied folders
        if intent.Item.Type = Folder && not (copied |> List.contains intent) then
            return! deleteEmptyFolders fs intent.Dest
        else
            return ([], [])
    }

    let cancelledUndo =
        if model.CancelToken.IsCancelled
        then Some (PutItems (Copy, intent, (copied |> List.except succeeded), true))
        else None
    let status = option {
        if not errors.IsEmpty then
            return MainStatus.Error (MainStatus.PutError (true, Copy, errors, copied.Length))
        return!
            deleteFolderErrors
            |> List.tryHead
            |> Option.map (fun (path, ex) -> MainStatus.Error (MainStatus.CouldNotDeleteCopyDest (path.Name, ex)))
        if model.CancelToken.IsCancelled then
            return MainStatus.Message (MainStatus.CancelledPut (Copy, true, succeeded.Length, copied.Length))
    }
    let refreshIfAtDest model =
        if model.Location = intent.Dest.Parent
        then Nav.refresh fs model |> Result.toOption |? model
        else model
    yield
        model
        |> Option.foldBack (fun a model -> model.WithPushedUndo a) cancelledUndo
        |> fun model -> model.WithPushedRedo (PutItems (Copy, intent, succeeded, model.CancelToken.IsCancelled))
        |> fun model ->
            { model with
                History = model.History.WithPathsRemoved ((succeeded |> List.map (fun pi -> pi.Dest)) @ deletedFolders)
            }
        |> Option.foldBack (fun s model -> model.WithStatus s) status
        |> refreshIfAtDest
}

let undoShortcut (fs: IFileSystem) shortcutPath (model: MainModel) = result {
    let action = DeletedItem ({ Item.Empty with Path = shortcutPath; Name = shortcutPath.Name; Type = File }, true)
    do! fs.Delete File shortcutPath |> itemActionError action model.PathFormat
    let model = { model with History = model.History.WithPathRemoved shortcutPath }
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

let rec private enumerateDeleteItems (fsReader: IFileSystemReader) (cancelToken: CancelToken) (items: Item seq) = asyncSeq {
    for item in items do
        if not cancelToken.IsCancelled then
            match item.Type with
            | Folder ->
                let! subItems = runAsync (fun () -> fsReader.GetItems item.Path)
                match subItems with
                | Ok subItems when not subItems.IsEmpty ->
                    yield! enumerateDeleteItems fsReader cancelToken subItems
                | _ -> ()
                yield item
            | File ->
                yield item
            | _ -> ()
}

let private removeItem item (model: MainModel) =
    { model with
        Directory = model.Directory |> List.except [item]
        Items = model.Items |> List.except [item] |> model.ItemsOrEmpty
        History = model.History.WithPathRemoved item.Path
    }.WithCursor model.Cursor

let delete (fs: IFileSystem) (progress: Event<float option>) item (model: MainModel) = asyncSeqResult {
    if item.Type.CanModify then
        let action = DeletedItem (item, true)
        let model = model.WithNewCancelToken()
        yield model.WithBusy (MainStatus.PreparingDelete item.Name)
        progress.Trigger (Some 0.0)
        let! items = enumerateDeleteItems fs model.CancelToken [item] |> AsyncSeq.toListAsync
        let incrementProgress = progressIncrementer progress items.Length
        yield model.WithBusy (MainStatus.DeletingItem (item, true))
        let! deleteResult = runAsync (fun () ->
            items
            |> Seq.takeWhile (fun _ -> not model.CancelToken.IsCancelled)
            |> Seq.map (fun i ->
                fs.Delete i.Type i.Path
                |> Result.mapError(fun ex -> (i, ex))
                |>! incrementProgress
            )
            |> Result.accumulate
            |> Result.map List.length
            |> Result.mapError (fun errors -> MainStatus.DeleteError (errors, items.Length))
        )
        progress.Trigger None
        let! deletedCount = deleteResult
        yield
            if model.CancelToken.IsCancelled then
                let model = model.WithMessage(MainStatus.CancelledDelete (true, deletedCount, items.Length))
                if deletedCount > 0 then
                    model.WithPushedUndo action
                    |> fun m -> { m with RedoStack = [action] }
                else
                    model
            else
                model
                |> removeItem item
                |> performedAction action
}

let private calculateTotalSize (fsReader: IFileSystemReader) (cancelToken: CancelToken) items =
    let rec iter (items: Item seq) =
        if cancelToken.IsCancelled then
            Ok 0L
        else
            items
            |> Seq.map (fun item ->
                match item.Type with
                | Folder -> fsReader.GetItems item.Path |> Result.bind iter
                | _ -> Ok (item.Size |? 0L)
            )
            |> Seq.fold (Result.map2 (+)) (Ok 0L)
    runAsync (fun () -> iter items)

let recycle (fs: IFileSystem) item (model: MainModel) = asyncSeqResult {
    if item.Type = NetHost then
        yield
            model
            |> removeItem item
            |> fun m -> m.WithMessage (MainStatus.RemovedNetworkHost item.Name)
    else if item.Type.CanModify then
        let action = DeletedItem (item, false)
        let model = model.WithNewCancelToken()
        yield model.WithBusy MainStatus.CheckingIsRecyclable
        let! sizeRes = calculateTotalSize fs model.CancelToken [item]
        let! size = sizeRes |> actionError "check folder content size"
        if model.CancelToken.IsCancelled then
            yield model.WithMessage (MainStatus.CancelledDelete (false, 0, 1))
        else
            yield model.WithBusy (MainStatus.DeletingItem (item, false))
            let! res = runAsync (fun () -> fs.Recycle size item.Type item.Path)
            do! res |> itemActionError action model.PathFormat
            yield
                model
                |> removeItem item
                |> performedAction action
}

let rec private undoIter iter fs progress model = asyncSeqResult {
    match model.UndoStack with
    | action :: rest ->
        let oldRedoHead = model.RedoStack |> List.tryHead
        let model = { model.ClearStatus () with UndoStack = rest }
        yield model
        let action =
            match action with
            | PutItems (Copy, intent, actual, _) ->
                // skip items that existed before the copy
                PutItems (Copy, intent, actual |> List.filter (fun pi -> not pi.DestExists), false)
            | PutItems (putType, intent, actual, true) ->
                PutItems (putType, intent, actual, false)
            | _ -> action
        let! model =
            match action with
            | CreatedItem item ->
                undoCreate fs item model
            | RenamedItem (oldItem, curName) ->
                undoRename fs oldItem curName model
                |> AsyncSeq.singleton
            | PutItems (Move, intent, actual, _) ->
                undoMove fs progress intent actual model
            | PutItems (Copy, intent, actual, _) ->
                undoCopy fs progress intent actual model
            | PutItems (Shortcut, intent, _, _) ->
                undoShortcut fs intent.Dest model
                |> AsyncSeq.singleton
            | DeletedItem (item, permanent) ->
                Error (MainStatus.CannotUndoDelete (permanent, item))
                |> AsyncSeq.singleton
        let model =
            if model.RedoStack |> List.tryHead = oldRedoHead
            then { model with RedoStack = action :: model.RedoStack }
            else model
        if iter < model.RepeatCount then
            yield! undoIter (iter + 1) fs progress model
        else if not model.IsStatusError && not model.IsStatusCancelled then
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
        let redoHead = model.RedoStack |> List.tryHead
        yield model
        let openPath (path: Path) =
            if path <> model.Location
            then Nav.openPath fs path SelectNone model
            else Ok model
        let! model = asyncSeqResult {
            match action with
            | CreatedItem item ->
                let! model = openPath item.Path.Parent
                yield! create fs item.Type item.Name model
            | RenamedItem (item, newName) ->
                let! model = openPath item.Path.Parent
                yield! rename fs item newName model
            | PutItems (putType, intent, _, cancelled) ->
                let! model = openPath intent.Dest.Parent
                let overwrite = intent.DestExists
                if not overwrite && not cancelled then
                    match! fs.GetItem intent.Dest |> actionError "check destination path" with
                    | Some existing ->
                        yield Nav.select (SelectItem (existing, true)) model
                        return MainStatus.CannotRedoPutToExisting (putType, intent.Item, intent.Dest.Format model.PathFormat)
                    | None -> ()
                yield! putToDestination fs progress true overwrite putType intent.Item intent.Dest model
            | DeletedItem (item, permanent) ->
                let! model =
                    openPath item.Path.Parent
                    |> Result.map (Nav.select (SelectItem (item, true)))
                yield model.WithBusy (MainStatus.RedoingDeletingItem (item, permanent))
                let deleteFunc = if permanent then delete fs progress else recycle fs
                yield! deleteFunc item model
        }
        // restore redo stack after operation with new item if present
        let newRedoItem = model.RedoStack |> List.tryHead |> Option.filter (fun action -> Some action <> redoHead)
        let model = { model with RedoStack = (newRedoItem |> Option.toList) @ rest }
        if iter < model.RepeatCount then
            yield! redoIter (iter + 1) fs progress model
        else
            match model.Status with
            | Some (MainStatus.Message (MainStatus.ActionComplete (action, _))) ->
                yield model.WithMessage (MainStatus.RedoAction (action, model.PathFormat, model.RepeatCount))
            | _ ->
                yield model
    | [] -> return MainStatus.NoRedoActions
}

let redo fs = redoIter 1 fs
