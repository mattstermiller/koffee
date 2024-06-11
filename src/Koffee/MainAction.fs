module Koffee.Main.Action

open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util

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

let selectToggle (model: MainModel) =
    let cursorItem = model.CursorItem
    let toggle, selectedItems =
        match model.SelectedItems |> List.partition ((=) cursorItem) with
        | [], _ -> (true, model.SelectedItems @ [cursorItem])
        | _, withoutCursorItem -> (false, withoutCursorItem)
    { model with
        SelectedItems = selectedItems
        PreviousSelectIndexAndToggle = Some (model.Cursor, toggle)
    }

let selectRange (model: MainModel) =
    let items = model.Items |> List.toArray
    match model.PreviousSelectIndexAndToggle with
    | Some (prevIndex, toggle) ->
        let rangeItems =
            [prevIndex; model.Cursor]
            |> List.sort
            |> fun indexes -> items.[indexes.[0]..indexes.[1]]
            |> Seq.toList
        let selectedItems =
            if toggle
            then model.SelectedItems @ rangeItems |> List.distinct
            else model.SelectedItems |> List.except rangeItems
        { model with
            SelectedItems = selectedItems
            PreviousSelectIndexAndToggle = Some (model.Cursor, toggle)
        }
    | None ->
        selectToggle model

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
        yield! Nav.openPath fs model.Location (CursorToPath (itemPath, false)) model
    | Some existing ->
        yield! Nav.openPath fs model.Location (CursorToPath (itemPath, true)) model
        return MainStatus.CannotUseNameAlreadyExists ("create", itemType, name, existing.IsHidden)
}

let undoCreate (fs: IFileSystem) undoIter item (model: MainModel) = asyncSeqResult {
    if not (fs.IsEmpty item.Path) then
        return MainStatus.CannotUndoNonEmptyCreated item
    yield model |> MainModel.withBusy (MainStatus.UndoingCreate item)
    let! res = runAsync (fun () -> fs.Delete item.Type item.Path)
    do! res |> itemActionError (DeletedItems (true, [item], false)) model.PathFormat
    yield
        model
        |> MainModel.mapHistory (History.withoutPaths [item.Path])
        |> performedUndo undoIter (CreatedItem item)
        |> applyIf (model.Location = item.Path.Parent) (ignoreError (Nav.refresh fs))
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
                    else Nav.listDirectory (CursorToPath (newPath, false)) model
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
            |> ignoreError (Nav.openPath fs parentPath (CursorToPath (oldItem.Path, false)))
    | Some existingItem ->
        return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", oldItem.Type, oldItem.Name, existingItem.IsHidden)
}

let registerSelectedItems putType (model: MainModel) =
    let regItems =
        model.ActionItems |> List.choose (fun item ->
            if item.Type.CanModify
            then Some item.Ref
            else None
        )
    match regItems |> List.tryHead with
    | Some itemRef ->
        { model with
            Config = { model.Config with YankRegister = Some (itemRef, putType) }
            Status = None
        }
    | None -> model

let getCopyName name i =
    let (nameNoExt, ext) = Path.SplitName name
    let number = if i = 0 then "" else string (i+1)
    sprintf "%s copy%s%s" nameNoExt number ext

let rec private enumeratePutItems (fsReader: IFileSystemReader) (cancelToken: CancelToken) deepExistsCheck copy
        copyNameToReuse destParent srcRefs =
    let rec iter checkExists (destParent: Path) (srcRefs: ItemRef seq) = asyncSeq {
        // TODO: try to add For support to asyncSeqResult to clean this up
        let existingNames =
            if checkExists then
                fsReader.GetItems destParent
                |> Result.map (List.map (fun i -> i.Name.ToLower()) >> Set)
                |> Result.mapError (fun e -> (destParent, e))
            else
                Ok (Set [])
        match existingNames with
        | Error e ->
            yield Error e
        | Ok existingNames ->
            let nameExists name =
                existingNames |> Set.contains (name |> String.toLower)
            for { Path = src; Type = typ } in srcRefs do
                if not cancelToken.IsCancelled then
                    let destNameAndExistsResult =
                        if src.Parent = destParent then
                            copyNameToReuse
                            |> Option.map (fun name -> Ok (name, nameExists name))
                            |> Option.defaultWith (fun () ->
                                Seq.init 99 (getCopyName src.Name)
                                |> Seq.tryFind (not << nameExists)
                                |> function
                                    | Some name -> Ok (name, false)
                                    | None -> Error (src, TooManyCopiesOfNameException src.Name :> exn)
                            )
                        else
                            Ok (src.Name, nameExists src.Name)
                    match destNameAndExistsResult with
                    | Error e ->
                        yield Error e
                    | Ok (destName, destExists) ->
                        let dest = destParent.Join destName
                        let putItem = { ItemType = typ; Source = src; Dest = dest; DestExists = destExists }
                        match typ with
                        | Folder when (destExists && deepExistsCheck) || not destExists && (copy || putItem.AreBasePathsDifferent) ->
                            let! itemsResult = runAsync (fun () -> fsReader.GetItems src)
                            match itemsResult with
                            | Ok items ->
                                yield Ok putItem
                                if not items.IsEmpty then
                                    yield! iter destExists dest (items |> Seq.map (fun i -> i.Ref))
                            | Error error ->
                                yield Error (src, error)
                        | Folder | File ->
                            yield Ok putItem
                        | _ -> ()
    }
    iter true destParent srcRefs

let private performPutItems (fs: IFileSystem) (progress: Progress) (cancelToken: CancelToken) isUndo putType (items: PutItem list) =
    let fileSysAction putItem =
        // when undoing a move that was an overwrite, copy it back since a version of the item was there before
        // (undo only calls performPutItems for Move)
        let putType = if isUndo && putItem.DestExists then Copy else putType
        match putType with
        | Shortcut ->
            fs.CreateShortcut putItem.Source putItem.Dest
        | _ when not isUndo && putItem.ItemType = Folder && putItem.DestExists ->
            Ok () // skip if dest folder already exists
        | Move when putItem.ItemType = Folder && putItem.AreBasePathsDifferent ->
            fs.Create putItem.ItemType putItem.Dest
        | Move ->
            fs.Move putItem.ItemType putItem.Source putItem.Dest
        | Copy when putItem.ItemType = Folder ->
            fs.Create putItem.ItemType putItem.Dest
        | Copy ->
            fs.Copy putItem.ItemType putItem.Source putItem.Dest
    let incrementProgress = progress.GetIncrementer items.Length
    runAsync (fun () ->
        items
        |> Seq.takeWhile (fun _ -> not cancelToken.IsCancelled)
        |> Seq.map (fun originalPutItem ->
            let putItem = originalPutItem |> applyIf isUndo PutItem.reverse
            fileSysAction putItem
            |> Result.map (fun () -> originalPutItem)
            |> Result.mapError (fun e -> (putItem.Source, e))
            |>! incrementProgress
        )
        |> Seq.toList
        |>! progress.Finish
    )

// TODO: failure to remove source folders should give an error to the user
// rename to deleteSourceFolders and yield error for folders that have files
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

let private performPut (fs: IFileSystem) progress undoIter enumErrors putType (intent: PutIntent) items (model: MainModel) =
    asyncSeqResult {
        let isUndo = undoIter |> Option.isSome
        let! results = items |> performPutItems fs progress model.CancelToken isUndo putType
        let succeeded, putErrors = results |> Result.partition
        let errorPaths = enumErrors @ putErrors
        let enumeratedItemCount = items.Length + enumErrors.Length

        // if nothing succeeded, return error
        if succeeded |> List.isEmpty then
            return MainStatus.PutError (isUndo, putType, errorPaths, enumeratedItemCount)

        let! deletedFolders, deleteFolderErrors = async {
            let isSubPathOf ancestor (path: Path) =
                path <> ancestor && path.IsWithin ancestor
            if putType = Move && intent.Source.Type = Folder
                    && succeeded |> List.exists (fun putItem -> putItem.Source |> isSubPathOf intent.Source.Path) then
                return! deleteEmptyFolders fs (if isUndo then intent.GetDest false else intent.Source.Path)
            else
                return ([], [])
        }

        let action = PutItems (putType, intent, succeeded, model.CancelToken.IsCancelled)
        let unsuccessful = lazy (items |> List.except succeeded)

        let updateUndoRedo (model: MainModel) =
            let cancelledAction =
                if model.CancelToken.IsCancelled then
                    Some (PutItems (putType, intent, unsuccessful.Value, true))
                else
                    None
            if isUndo then
                model
                |> Option.foldBack MainModel.pushUndo cancelledAction
                |> MainModel.pushRedo action
            else
                model
                |> MainModel.pushUndo action
                |> MainModel.withRedoStack (cancelledAction |> Option.toList)
        let updateHistory model =
            if putType = Move then
                let shouldReplaceHistory overwritten = not (isUndo && overwritten)
                let getReplacement originalSource originalDest =
                    if isUndo
                    then originalDest, originalSource
                    else originalSource, originalDest
                let isIncompleteEnumeratedFolder putItem =
                    putItem.ItemType = Folder
                    && (putItem.DestExists || putItem.AreBasePathsDifferent)
                    && (unsuccessful.Value |> List.exists (fun pi -> pi.Source.IsWithin putItem.Source)
                        || enumErrors |> List.exists (fun (path, _) -> path.IsWithin putItem.Source || path.IsWithin putItem.Dest)
                    )
                let isCompleted = errorPaths.IsEmpty && not model.CancelToken.IsCancelled
                let pathReplacements =
                    if isCompleted && shouldReplaceHistory intent.Overwrite then
                        Map [getReplacement intent.Source.Path (intent.GetDest false)]
                    else
                        succeeded
                        |> List.filter (fun putItem ->
                            shouldReplaceHistory putItem.DestExists && not (isIncompleteEnumeratedFolder putItem)
                        )
                        |> List.map (fun putItem -> getReplacement putItem.Source putItem.Dest)
                        |> Map
                model |> MainModel.mapHistory (
                    History.withPathsReplaced pathReplacements
                    >> History.withoutPaths deletedFolders
                )
            else
                model
        let openDest model =
            let dest =
                if isUndo then
                    intent.Source.Path
                else
                    intent.FilterPutItemsToDestParent succeeded
                    |> Seq.map (fun putItem -> putItem.Dest)
                    |> Seq.tryHead
                    |> Option.defaultWith (fun () ->
                        let rec childOfDestParent (path: Path) =
                            if path.Parent = intent.DestParent then path
                            else childOfDestParent path.Parent
                        childOfDestParent succeeded.Head.Dest
                    )
            model |> ignoreError (Nav.openPath fs dest.Parent (CursorToPath (dest, false)))
        let status =
            // for partial success, set error message instead of returning Error so the caller flow is not short-circuited
            if not errorPaths.IsEmpty then
                MainStatus.Error (MainStatus.PutError (isUndo, putType, errorPaths, enumeratedItemCount))
            else if model.CancelToken.IsCancelled then
                MainStatus.Message (MainStatus.CancelledPut (putType, isUndo, succeeded.Length, items.Length))
            else
                deleteFolderErrors
                |> List.tryHead
                |> Option.map (fun (path, ex) ->
                    MainStatus.Error (MainStatus.CouldNotDeleteMoveSource (path.Name, ex))
                )
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

let putToDestination (fs: IFileSystem) (progress: Progress) isRedo putType (intent: PutIntent) model = asyncSeqResult {
    let model = { model with CancelToken = CancelToken() }
    yield model |> MainModel.withBusy (MainStatus.PreparingPut (putType, intent.Source.Path.Name))
    progress.Start ()
    let alreadyPutItemsForResume =
        if isRedo then
            model.UndoStack |> List.tryHead |> (function
                | Some (PutItems (undoPutType, undoIntent, actual, true))
                        when undoPutType = putType && PutIntent.equalSourceAndDest undoIntent intent ->
                    actual
                | _ -> []
            )
        else []
    let copyNameToReuse =
        if intent.Source.Path.Parent = intent.DestParent then
            intent.FilterPutItemsToDestParent alreadyPutItemsForResume
            |> Seq.map (fun putItem -> putItem.Dest.Name)
            |> Seq.tryHead
        else None
    let! enumerated = async {
        match putType with
        | Move | Copy ->
            let deepExistsCheck = intent.Overwrite || not alreadyPutItemsForResume.IsEmpty
            return!
                enumeratePutItems fs model.CancelToken deepExistsCheck (putType = Copy) copyNameToReuse
                    intent.DestParent [intent.Source]
                |> AsyncSeq.toListAsync
        | Shortcut ->
            let destPath = intent.GetDest true
            match fs.GetItem destPath with
            | Error e ->
                return [Error (destPath, e)]
            | Ok existing ->
                let putItem = {
                    ItemType = intent.Source.Type
                    Source = intent.Source.Path
                    Dest = destPath
                    DestExists = existing.IsSome
                }
                return [Ok putItem]
    }
    let pathToConfirmOverwrite =
        if not intent.Overwrite && not isRedo then
            enumerated |> List.tryPick (function
                | Ok putItem when putItem.DestExists -> Some putItem.Dest
                | _ -> None
            )
        else
            None
    match pathToConfirmOverwrite with
    | Some existingPath ->
        // refresh item list to make sure we can see the existing file
        let! existing =
            fs.GetItem existingPath
            |> Result.bind (Result.ofOption (exn "existing item missing")) // shouldn't ever happen :)
            |> actionError "read existing item"
        let! sourceItem =
            fs.GetItem intent.Source.Path
            |> Result.bind (Result.ofOption (exn "source item missing"))
            |> actionError "read source item"
        let! model = Nav.openPath fs model.Location (CursorToPath (existingPath, true)) model
        yield
            { model with
                InputMode = Some (Confirm (Overwrite (putType, sourceItem, existing)))
                InputText = ""
            }
    | None ->
        if model.CancelToken.IsCancelled then
            yield model |> MainModel.withMessage (MainStatus.CancelledPut (putType, false, 0, enumerated.Length))
        else
            // if resuming cancelled put, skip items already done
            let filterItemsToSkip =
                alreadyPutItemsForResume
                |> List.map (fun putItem -> putItem.Source)
                |> function
                    | [] -> id
                    | pathsToSkip ->
                        List.filter (fun (res: Result<PutItem, _>) -> // TODO: remove type annotation
                            let path = res |> Result.toOption |> Option.map (fun putItem -> putItem.Source)
                            not (path |> Option.exists (Seq.containedIn pathsToSkip))
                        )
            let blockRedoIfDestExists putResult =
                putResult |> Result.bind (fun putItem ->
                    if putItem.DestExists
                    then Error (putItem.Source, RedoPutBlockedByExistingItemException() :> exn)
                    else Ok putItem
                )
            let items, enumErrors =
                enumerated
                |> filterItemsToSkip
                |> applyIf (isRedo && not intent.Overwrite) (List.map blockRedoIfDestExists)
                |> Result.partition
            if putType = Move || putType = Copy then
                yield model |> MainModel.withBusy (MainStatus.PuttingItem ((putType = Copy), isRedo, intent, model.PathFormat))
            yield! performPut fs progress None enumErrors putType intent items model
}

let putInLocation (fs: IFileSystem) progress isRedo overwrite putType (itemRef: ItemRef) model = asyncSeqResult {
    do! fs.GetItem model.Location
        |> actionError "read put location"
        |> Result.okIf (Option.exists (fun l -> l.Type.CanCreateIn)) MainStatus.CannotPutHere
        |> Result.map ignore
    let sameFolder = itemRef.Path.Parent = model.Location
    if putType = Move && sameFolder then
        return MainStatus.CannotMoveToSameFolder
    let intent = { Source = itemRef; DestParent = model.Location; Overwrite = overwrite }
    yield! putToDestination fs progress isRedo putType intent model
}

let put (fs: IFileSystem) progress overwrite (model: MainModel) = asyncSeqResult {
    match model.Config.YankRegister with
    | None -> ()
    | Some (itemRef, putType) ->
        if model.IsSearchingSubFolders then
            return MainStatus.CannotPutHere
        let! model = putInLocation fs progress false overwrite putType itemRef model
        let wasImmediatelyCanceled =
            match model.Status with Some (MainStatus.Message (MainStatus.CancelledPut (_, _, 0, _))) -> true | _ -> false
        // if not cancelled or opened input for confirmation, clear yank register
        if not wasImmediatelyCanceled && model.InputMode.IsNone then
            yield { model with Config = { model.Config with YankRegister = None } }
}

let undoMove (fs: IFileSystem) progress undoIter (intent: PutIntent) (moved: PutItem list) (model: MainModel) =
    asyncSeqResult {
        let model = { model with CancelToken = CancelToken() }
        yield model |> MainModel.withBusy (MainStatus.UndoingPut (false, intent, model.PathFormat))
        let! items, existErrors = runAsync (fun () ->
            moved
            |> Seq.takeWhile (fun _ -> not model.CancelToken.IsCancelled)
            |> Seq.map (fun putItem ->
                match fs.GetItem putItem.Source with
                | Ok (Some _) when putItem.ItemType <> Folder ->
                    Error (putItem.Dest, UndoMoveBlockedByExistingItemException() :> exn)
                | Ok _ ->
                    Ok putItem
                | Error e ->
                    Error (putItem.Dest, e)
            )
            |> Result.partition
        )
        if not model.CancelToken.IsCancelled then
            yield! performPut fs progress (Some undoIter) existErrors Move intent items model
    }

let private performUndoCopy (fs: IFileSystem) (progress: Progress) (cancelToken: CancelToken) (putItems: PutItem list) =
    let incrementProgress = progress.GetIncrementer putItems.Length
    runAsync (fun () ->
        putItems
        |> Seq.takeWhile (fun _ -> not cancelToken.IsCancelled)
        |> Seq.map (fun putItem ->
            fs.Delete putItem.ItemType putItem.Dest
            |> Result.map (fun () -> putItem)
            |> Result.mapError (fun e -> (putItem.Dest, e))
            |>! incrementProgress
        )
        |> Seq.toList
        |>! progress.Finish
    )

let undoCopy fs progress undoIter intent copied (model: MainModel) = asyncSeqResult {
    // skip items that existed before the copy
    let itemsToDelete = copied |> List.filter (fun pi -> not pi.DestExists)
    let model = { model with CancelToken = CancelToken() }
    yield model |> MainModel.withBusy (MainStatus.UndoingPut (true, intent, model.PathFormat))
    // delete items in reverse order so that parent folders are deleted after their children
    let! results = performUndoCopy fs progress model.CancelToken (itemsToDelete |> List.rev)
    let succeeded, errors = results |> Result.partition |> mapFst List.rev

    if not errors.IsEmpty && succeeded.IsEmpty then
        return MainStatus.PutError (true, Copy, errors, itemsToDelete.Length)

    let cancelledUndo =
        if model.CancelToken.IsCancelled
        then Some (PutItems (Copy, intent, (itemsToDelete |> List.except succeeded), true))
        else None
    let action = PutItems (Copy, intent, succeeded, model.CancelToken.IsCancelled)
    let status =
        if not errors.IsEmpty then
            MainStatus.Error (MainStatus.PutError (true, Copy, errors, itemsToDelete.Length))
        else if model.CancelToken.IsCancelled then
            MainStatus.Message (MainStatus.CancelledPut (Copy, true, succeeded.Length, itemsToDelete.Length))
        else
            MainStatus.Message (MainStatus.UndoAction (action, model.PathFormat, undoIter, model.RepeatCount))
    let pathHistoryToRemove = (succeeded |> List.map (fun pi -> pi.Dest))
    yield
        model
        |> Option.foldBack MainModel.pushUndo cancelledUndo
        |> MainModel.pushRedo action
        |> MainModel.withStatus status
        |> MainModel.mapHistory (History.withoutPaths pathHistoryToRemove)
        |> applyIf (model.Location = intent.DestParent) (ignoreError (Nav.refresh fs))
}

let undoShortcut (fs: IFileSystem) undoIter oldAction shortcutPath (model: MainModel) = result {
    let item = Item.Basic shortcutPath shortcutPath.Name File
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

let private performDelete (fs: IFileSystem) (progress: Progress) permanent items (enumerated: Item list) (model: MainModel) = asyncSeqResult {
    yield model |> MainModel.withBusy (MainStatus.DeletingItems (permanent, items))
    let totalCount = enumerated.Length
    let incrementProgress = progress.GetIncrementer totalCount
    let deleteFunc = if permanent then fs.Delete else fs.Recycle
    let! results = runAsync (fun () ->
        enumerated
        |> Seq.takeWhile (fun _ -> not model.CancelToken.IsCancelled)
        |> Seq.map (fun item ->
            deleteFunc item.Type item.Path
            |> Result.map (fun () -> item)
            |> Result.mapError (fun ex -> (item.Path, ex))
            |>! incrementProgress
        )
    )

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

    progress.Finish ()
    yield
        model
        |> applyIf (deletedCount > 0) (
            removeItems itemsDeleted
            >> Option.foldBack MainModel.pushUndo undoAction
            >> MainModel.withRedoStack (resumeAction |> Option.toList)
        )
        |> MainModel.withStatus status
}

let delete (fs: IFileSystem) (progress: Progress) items (model: MainModel) = asyncSeqResult {
    let model = model |> MainModel.withNewCancelToken
    yield model |> MainModel.withBusy (MainStatus.PreparingDelete items)
    progress.Start ()
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

let recycle (fs: IFileSystem) (progress: Progress) (items: Item list) (model: MainModel) = asyncSeqResult {
    if items.Head.Type = NetHost then
        yield
            model
            |> removeItems items
            |> MainModel.withMessage (MainStatus.RemovedNetworkHosts (items |> List.map (fun i -> i.Name)))
    else
        let items = items |> List.filter (fun i -> i.Type.CanModify)
        let model = model |> MainModel.withNewCancelToken
        yield model |> MainModel.withBusy MainStatus.CheckingIsRecyclable
        progress.Start ()
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
            | PutItems (Shortcut, intent, actual, _) ->
                undoShortcut fs iter action actual.Head.Dest model
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
        let openPath (path: Path) cursorMove =
            if path <> model.Location
            then Nav.openPath fs path cursorMove model
            else Ok model
        let! model = asyncSeqResult {
            match action with
            | CreatedItem item ->
                let! model = openPath item.Path.Parent CursorStay
                yield! create fs item.Type item.Name model
            | RenamedItem (item, newName) ->
                let! model = openPath item.Path.Parent CursorStay
                yield! rename fs item newName model
            | PutItems (putType, intent, _, _) ->
                let! model = openPath intent.DestParent CursorStay
                yield! putToDestination fs progress true putType intent model
            | DeletedItems (permanent, items, _) ->
                // normally, redo of delete is impossible because undo is impossible, but cancellation pushes redo action for resuming
                let cursor = CursorToAndSelectPaths (items |> List.map (fun i -> i.Path), true)
                let! model = openPath items.Head.Path.Parent cursor
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
