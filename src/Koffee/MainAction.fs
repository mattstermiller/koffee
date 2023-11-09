module Koffee.Main.Action

open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util

// TODO: refactor to Progress class with Start(total), Increment, Finish
let private progressIncrementer (progress: Event<float option>) total =
    let incr = 1.0 / float total
    fun _ -> progress.Trigger (Some incr)

let private performedAction action (model: MainModel) =
    model
    |> MainModel.pushUndo action
    |> MainModel.withRedoStack []
    |> MainModel.withMessage (MainStatus.ActionComplete (action, model.PathFormat))

let private performedUndo undoIter action (model: MainModel) =
    model
    |> MainModel.pushRedo action
    |> MainModel.withMessage (MainStatus.UndoAction (action, model.PathFormat, undoIter, model.RepeatCount))

let private ignoreError f model =
    f model |> Result.defaultValue model

let private getInputSelection renamePart itemType (input: string) =
    let fullLen = input.Length
    let nameLen =
        if itemType = File
        then Path.SplitName input |> fst |> String.length
        else fullLen
    match renamePart with
    | Begin -> (0, 0)
    | EndName -> (nameLen, 0)
    | End -> (fullLen, 0)
    | ReplaceName -> (0, nameLen)
    | ReplaceAll -> (0, fullLen)

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
        | Confirm Delete -> Ok (model.ActionItems |> List.exists (fun item -> item.Type.CanModify))
        | _ -> Ok true
    if allowed then
        let model = { model with InputMode = Some inputMode }
        let setInputText renamePart itemType input model =
            { model with
                InputText = input
                InputTextSelection = getInputSelection renamePart itemType input
            }
        match inputMode with
        | Input Search ->
            let input = model.SearchCurrent |> Option.map (fun s -> s.Terms) |? ""
            return model |> setInputText ReplaceAll Empty input
        | Input (Rename part) ->
            return model |> setInputText part model.CursorItem.Type model.CursorItem.Name
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
        yield! Nav.openPath fs model.Location (CursorToName name) model
    | Some existing ->
        yield! Nav.openPath fs model.Location (CursorToItem (existing, true)) model
        return MainStatus.CannotUseNameAlreadyExists ("create", itemType, name, existing.IsHidden)
}

let undoCreate (fs: IFileSystem) undoIter item (model: MainModel) = asyncSeqResult {
    if not (fs.IsEmpty item.Path) then
        return MainStatus.CannotUndoNonEmptyCreated item
    yield model |> MainModel.withBusy (MainStatus.UndoingCreate item)
    let! res = runAsync (fun () -> fs.Delete item.Type item.Path)
    do! res |> itemActionError (DeletedItems (true, [item], false)) model.PathFormat
    let model =
        model
        |> MainModel.mapHistory (History.withoutPaths [item.Path])
        |> performedUndo undoIter (CreatedItem item)
    if model.Location = item.Path.Parent then
        yield model |> ignoreError (Nav.refresh fs)
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
                    History = model.History |> History.withPathReplaced item.Path newPath
                }
                |> fun model ->
                    if model.SearchCurrent.IsSome
                    then { model with Items = model.Items |> substitute }
                    else Nav.listDirectory (CursorToName newName) model
                |> performedAction action
        | Some existingItem ->
            return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", item.Type, newName, existingItem.IsHidden)
    else return model
}

let undoRename (fs: IFileSystem) undoIter oldItem currentName (model: MainModel) = result {
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
        return
            model
            |> MainModel.mapHistory (History.withPathReplaced item.Path oldItem.Path)
            |> performedUndo undoIter (RenamedItem (oldItem, currentName))
            |> ignoreError (Nav.openPath fs parentPath (CursorToName oldItem.Name))
    | Some existingItem ->
        return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", oldItem.Type, oldItem.Name, existingItem.IsHidden)
}

let registerSelectedItems putType (model: MainModel) =
    let regItems =
        model.ActionItems |> List.choose (fun item ->
            if item.Type.CanModify
            then Some (item.Path, item.Type)
            else None
        )
    match regItems |> List.tryHead with
    | Some (path, itemType) ->
        { model with
            Config = { model.Config with YankRegister = Some (path, itemType, putType) }
            Status = None
        }
    | None -> model

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

let private performPut (fs: IFileSystem) (progress: Event<_>) (undoIter: int option) enumErrors putType intent items
        (model: MainModel) =
    asyncSeqResult {
        let isUndo = undoIter.IsSome
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

        let action =
            if isUndo then
                // if there were errors, destination folder still exists so set DestExists to allow redo to merge
                let redoIntent = if not errorItems.IsEmpty then { intent with DestExists = true } else intent
                getReversedAction redoIntent succeeded model.CancelToken.IsCancelled
            else
                PutItems (putType, intent, succeeded, model.CancelToken.IsCancelled)

        let updateUndoRedo (model: MainModel) =
            if isUndo then
                model
                |> Option.foldBack MainModel.pushUndo (cancelledAction true)
                |> MainModel.pushRedo action
            else
                model
                |> MainModel.pushUndo action
                |> MainModel.withRedoStack (cancelledAction false |> Option.toList)
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
                model |> MainModel.mapHistory (
                    History.withPathsReplaced pathReplacements
                    >> History.withoutPaths deletedFolders
                )
            else
                model
        let openDest model =
            model |> ignoreError (Nav.openPath fs intent.Dest.Parent (CursorToName intent.Dest.Name))
        let status =
            // for partial success, set error message instead of returning Error so the caller flow is not short-circuited
            if not errorItems.IsEmpty then
                MainStatus.Error (MainStatus.PutError (isUndo, putType, errorItems, items.Length + enumErrors.Length))
            else if model.CancelToken.IsCancelled then
                MainStatus.Message (MainStatus.CancelledPut (putType, isUndo, succeeded.Length, items.Length))
            else
                deleteFolderErrors
                |> List.tryHead
                |> Option.map (fun (path, ex) -> MainStatus.Error (MainStatus.CouldNotDeleteMoveSource (path.Name, ex)))
                |> Option.defaultWith (fun () ->
                    match undoIter with
                    | Some iter ->
                        MainStatus.Message (MainStatus.UndoAction (action, model.PathFormat, iter, model.RepeatCount))
                    | None ->
                        MainStatus.Message (MainStatus.ActionComplete (action, model.PathFormat))
                )
        yield
            model
            |> updateUndoRedo
            |> updateHistory
            |> MainModel.withStatus status
            |> openDest
    }

let putToDestination (fs: IFileSystem) (progress: Event<float option>) isRedo overwrite putType item destPath model = asyncSeqResult {
    let! existing = fs.GetItem destPath |> actionError "check destination path"
    match existing with
    | Some existing when not overwrite && not isRedo ->
        // refresh item list to make sure we can see the existing file
        let! model = Nav.openPath fs model.Location (CursorToItem (existing, true)) model
        yield
            { model with
                InputMode = Some (Confirm (Overwrite (putType, item, existing)))
                InputText = ""
            }
    | _ ->
        let model = { model with CancelToken = CancelToken() }
        let putItem = { Item = item; Dest = destPath; DestExists = existing.IsSome }
        yield model |> MainModel.withBusy (MainStatus.PreparingPut (putType, item.Name))
        progress.Trigger (Some 0.0)
        let! enumerated =
            match putType with
            | Shortcut ->
                async { return [Ok putItem] }
            | Move | Copy ->
                enumeratePutItems fs model.CancelToken (putType = Copy) destPath item |> AsyncSeq.toListAsync
        if model.CancelToken.IsCancelled then
            yield model |> MainModel.withMessage (MainStatus.CancelledPut (putType, false, 0, enumerated.Length))
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
                |> applyIf (isRedo && not overwrite) (List.map errorOnExisting)
                |> Result.partition
            if putType = Move || putType = Copy then
                yield model |> MainModel.withBusy (MainStatus.PuttingItem ((putType = Copy), isRedo, putItem, model.PathFormat))
            yield! performPut fs progress None enumErrors putType putItem items model
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

let undoMove (fs: IFileSystem) progress undoIter (intent: PutItem) (moved: PutItem list) (model: MainModel) =
    asyncSeqResult {
        let model = { model with CancelToken = CancelToken() }
        yield model |> MainModel.withBusy (MainStatus.UndoingPut (false, intent.Item))
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
            yield! performPut fs progress (Some undoIter) existErrors Move putItem items model
    }

let private performUndoCopy (fs: IFileSystem) progress (cancelToken: CancelToken) (putItems: PutItem list) =
    let incrementProgress = progressIncrementer progress putItems.Length
    let shouldDelete (putItem, currentItem) =
        let copyModified = currentItem |> Result.map (Option.bind (fun copiedItem -> copiedItem.Modified))
        match putItem.Item.Modified, copyModified with
        | origTime, Ok copyTime when origTime = copyTime -> true
        | _ -> false
    runAsync (fun () ->
        let deleteItems, recycleItemsAndSizes =
            putItems
            |> List.map (fun putItem -> (putItem, fs.GetItem putItem.Dest))
            |> List.partition shouldDelete
            |> fun (delete, recycle) ->
                let getSize (itemRes: Result<Item option, _>) =
                    itemRes |> Result.map (Option.bind (fun item -> item.Size) >> Option.defaultValue 0L)
                (delete |> List.map fst
                ,recycle |> List.map (mapSnd getSize))
        // TODO: when implementing multi-select for move and copy, write tests for:
        // total recycle limit reached should error on recycle items, deletes should proceed
        // error getting size for item should error on that item only, recycle should proceed if under limit
        let totalRecycleSizeResult =
            match recycleItemsAndSizes with
            | [] -> Ok ()
            | (firstItem, _) :: _ ->
                let totalSize = recycleItemsAndSizes |> List.choose (snd >> Result.toOption) |> List.sum
                fs.CheckRecyclable totalSize firstItem.Dest
        let processItems f items =
            items
            |> Seq.takeWhile (fun _ -> not cancelToken.IsCancelled)
            |> Seq.map f
            |> Seq.map (fun (putItem, res) ->
                res
                |> Result.map (fun () -> putItem)
                |> Result.mapError (fun e -> ({ putItem.Item with Path = putItem.Dest }, e))
                |>! incrementProgress
            )
        seq {
            yield!
                deleteItems |> processItems (fun putItem ->
                    (putItem, fs.Delete putItem.Item.Type putItem.Dest)
                )
            yield!
                recycleItemsAndSizes |> processItems (fun (putItem, sizeRes) ->
                    let processRes =
                        sizeRes
                        |> Result.bind (cnst totalRecycleSizeResult)
                        |> Result.bind (fun () -> fs.Recycle putItem.Item.Type putItem.Dest)
                    (putItem, processRes)
                )
        }
        |> Seq.toList
        |>! (fun _ -> progress.Trigger None)
    )

let undoCopy fs progress undoIter (intent: PutItem) copied (model: MainModel) = asyncSeqResult {
    // skip items that existed before the copy
    let copied = copied |> List.filter (fun pi -> not pi.DestExists)
    let model = { model with CancelToken = CancelToken() }
    yield model |> MainModel.withBusy (MainStatus.UndoingPut (true, intent.Item))
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
    let action = PutItems (Copy, intent, succeeded, model.CancelToken.IsCancelled)
    let status =
        if not errors.IsEmpty then
            MainStatus.Error (MainStatus.PutError (true, Copy, errors, copied.Length))
        else
            deleteFolderErrors
            |> List.tryHead
            |> Option.map (fun (path, ex) -> MainStatus.Error (MainStatus.CouldNotDeleteCopyDest (path.Name, ex)))
            |> Option.defaultWith (fun () ->
                if model.CancelToken.IsCancelled
                then MainStatus.Message (MainStatus.CancelledPut (Copy, true, succeeded.Length, copied.Length))
                else MainStatus.Message (MainStatus.UndoAction (action, model.PathFormat, undoIter, model.RepeatCount))
            )
    let pathHistoryToRemove = (succeeded |> List.map (fun pi -> pi.Dest)) @ deletedFolders
    let refreshIfAtDest model =
        if model.Location = intent.Dest.Parent
        then model |> ignoreError (Nav.refresh fs)
        else model
    yield
        model
        |> Option.foldBack MainModel.pushUndo cancelledUndo
        |> MainModel.pushRedo action
        |> MainModel.withStatus status
        |> MainModel.mapHistory (History.withoutPaths pathHistoryToRemove)
        |> refreshIfAtDest
}

let undoShortcut (fs: IFileSystem) undoIter oldAction shortcutPath (model: MainModel) = result {
    let item = { Item.Empty with Path = shortcutPath; Name = shortcutPath.Name; Type = File }
    let action = DeletedItems (true, [item], false)
    do! fs.Delete File shortcutPath |> itemActionError action model.PathFormat
    return
        model
        |> MainModel.mapHistory (History.withoutPaths [shortcutPath])
        |> performedUndo undoIter oldAction
        |> applyIf (model.Location = shortcutPath.Parent) (ignoreError (Nav.refresh fs))
}

let clipCopy (os: IOperatingSystem) (model: MainModel) = result {
    let items = model.ActionItems |> List.filter (fun item -> item.Type <> Empty)
    match items with
    | [] ->
        return model
    // TODO: support multiple items
    | item :: _ ->
        do! os.CopyToClipboard item.Path |> actionError "copy to clipboard"
        return model |> MainModel.withMessage (MainStatus.ClipboardCopy (item.Path.Format model.PathFormat))
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

let private removeItems (items: Item list) (model: MainModel) =
    if items.IsEmpty then
        model
    else
        let history =
            if items.Head.Type = NetHost
            then model.History |> History.withoutNetHosts (items |> List.map (fun i -> i.Name))
            else model.History |> History.withoutPaths (items |> Item.paths)
        { model with
            Directory = model.Directory |> List.except items
            Items = model.Items |> List.except items |> model.ItemsOrEmpty
            SelectedItems = model.SelectedItems |> List.except items
            History = history
        }
        |> MainModel.withCursor model.Cursor

let private performDelete (fs: IFileSystem) progress permanent items (enumerated: Item list) (model: MainModel) = asyncSeqResult {
    let totalCount = enumerated.Length
    let incrementProgress = progressIncrementer progress totalCount
    yield model |> MainModel.withBusy (MainStatus.DeletingItems (permanent, items))
    let deleteFunc = if permanent then fs.Delete else fs.Recycle
    let! results = runAsync (fun () ->
        enumerated
        |> Seq.takeWhile (fun _ -> not model.CancelToken.IsCancelled)
        |> Seq.map (fun item ->
            deleteFunc item.Type item.Path
            |> Result.map (fun () -> item)
            |> Result.mapError (fun ex -> (item, ex))
            |>! incrementProgress
        )
    )
    progress.Trigger None
    let actualDeleted, errors = results |> Result.partition
    let deletedCount = actualDeleted.Length
    let basePath = items.Head.Path.Parent
    let itemsDeleted = actualDeleted |> List.filter (fun i -> i.Path.Parent = basePath)

    let undoAction =
        if not itemsDeleted.IsEmpty then
            Some (DeletedItems (permanent, itemsDeleted, model.CancelToken.IsCancelled))
        else
            None
    let resumeAction =
        if model.CancelToken.IsCancelled && deletedCount > 0
        then Some (DeletedItems (permanent, items |> List.except itemsDeleted, true))
        else None
    let status =
        if not errors.IsEmpty then
            MainStatus.Error (MainStatus.DeleteError (permanent, errors, totalCount))
        else if model.CancelToken.IsCancelled then
            MainStatus.Message (MainStatus.CancelledDelete (permanent, deletedCount, totalCount))
        else
            MainStatus.Message (MainStatus.ActionComplete (DeletedItems (permanent, items, false), model.PathFormat))

    yield
        model
        |> applyIf (deletedCount > 0) (
            removeItems itemsDeleted
            >> Option.foldBack MainModel.pushUndo undoAction
            >> MainModel.withRedoStack (resumeAction |> Option.toList)
        )
        |> MainModel.withStatus status
}

let delete (fs: IFileSystem) (progress: Event<float option>) items (model: MainModel) = asyncSeqResult {
    let model = model |> MainModel.withNewCancelToken
    yield model |> MainModel.withBusy (MainStatus.PreparingDelete items)
    progress.Trigger (Some 0.0)
    let! enumerated = enumerateDeleteItems fs model.CancelToken items |> AsyncSeq.toListAsync
    yield! performDelete fs progress true items enumerated model
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

let recycle (fs: IFileSystem) (progress: Event<float option>) (items: Item list) (model: MainModel) = asyncSeqResult {
    if items.Head.Type = NetHost then
        yield
            model
            |> removeItems items
            |> MainModel.withMessage (MainStatus.RemovedNetworkHosts (items |> List.map (fun i -> i.Name)))
    else
        let items = items |> List.filter (fun i -> i.Type.CanModify)
        let model = model |> MainModel.withNewCancelToken
        yield model |> MainModel.withBusy MainStatus.CheckingIsRecyclable
        progress.Trigger (Some 0.0)
        let! totalSizeRes = calculateTotalSize fs model.CancelToken items
        let! totalSize = totalSizeRes |> actionError "check folder content size"
        do! fs.CheckRecyclable totalSize items.Head.Path |> actionError "recycle"
        yield! performDelete fs progress false items items model
}

let rec private undoIter iter fs progress model = asyncSeqResult {
    match model.UndoStack with
    | action :: rest ->
        let model = { model with UndoStack = rest }
        yield model
        let! model =
            match action with
            | CreatedItem item ->
                undoCreate fs iter item model
            | RenamedItem (oldItem, curName) ->
                undoRename fs iter oldItem curName model
                |> AsyncSeq.singleton
            | PutItems (Move, intent, actual, _) ->
                undoMove fs progress iter intent actual model
            | PutItems (Copy, intent, actual, _) ->
                undoCopy fs progress iter intent actual model
            | PutItems (Shortcut, intent, _, _) ->
                undoShortcut fs iter action intent.Dest model
                |> AsyncSeq.singleton
            | DeletedItems (permanent, items, _) ->
                Error (MainStatus.CannotUndoDelete (permanent, items))
                |> AsyncSeq.singleton
        if iter < model.RepeatCount && not model.IsStatusCancelled then
            yield! undoIter (iter + 1) fs progress model
        else
            yield model
    | [] ->
        return MainStatus.NoUndoActions
}

let undo fs = undoIter 1 fs

let rec private redoIter iter fs progress model = asyncSeqResult {
    match model.RedoStack with
    | action :: rest ->
        let model = { model with RedoStack = rest }
        yield model
        let redoHead = model.RedoStack |> List.tryHead
        let openPath (path: Path) =
            if path <> model.Location
            then Nav.openPath fs path CursorStay model
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
                        yield Nav.moveCursor (CursorToItem (existing, true)) model
                        return MainStatus.CannotRedoPutToExisting (putType, intent.Item, intent.Dest.Format model.PathFormat)
                    | None -> ()
                yield! putToDestination fs progress true overwrite putType intent.Item intent.Dest model
            | DeletedItems (permanent, items, _) ->
                let! model =
                    openPath items.Head.Path.Parent
                    |> Result.map (
                        Nav.moveCursor (CursorToItem (items.Head, true))
                        >> applyIf (not items.Tail.IsEmpty) (MainModel.selectItems items)
                    )
                yield model |> MainModel.withBusy (MainStatus.RedoingDeleting (permanent, items))
                let deleteFunc = if permanent then delete else recycle
                yield! deleteFunc fs progress model.ActionItems model
        }
        let newRedoItem = model.RedoStack |> List.tryHead |> Option.filter (fun action -> Some action <> redoHead)
        let status, statusHistory =
            match model.Status with
            | Some (MainStatus.Message (MainStatus.ActionComplete (action, _))) ->
                let status = MainStatus.Message (MainStatus.RedoAction (action, model.PathFormat, iter, model.RepeatCount))
                (Some status, status :: model.StatusHistory.Tail)
            | status -> (status, model.StatusHistory)
        let model =
            { model with
                // restore redo stack after operation with new item if present
                RedoStack = (newRedoItem |> Option.toList) @ rest
                Status = status
                StatusHistory = statusHistory
            }
        if iter < model.RepeatCount && not model.IsStatusCancelled then
            yield! redoIter (iter + 1) fs progress model
        else
            yield model
    | [] ->
        return MainStatus.NoRedoActions

}

let redo fs = redoIter 1 fs
