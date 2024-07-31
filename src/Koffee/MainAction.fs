module Koffee.Main.Action

open System.Collections.Generic
open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util
open System.Threading

let private performedAction action (model: MainModel) =
    model
    |> MainModel.pushUndo action
    |> MainModel.withRedoStack []
    |> MainModel.withMessage (MainStatus.ActionComplete action)

let private performedUndo undoIter action (model: MainModel) =
    model
    |> MainModel.pushRedo action
    |> MainModel.withMessage (MainStatus.UndoAction (action, undoIter, model.RepeatCount))

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
    let! existing = fs.GetItem itemPath |> itemActionError action
    match existing with
    | None ->
        do! fs.Create itemType itemPath |> itemActionError action
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
    do! res |> itemActionError (DeletedItems (true, [item], false))
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
            else fs.GetItem newPath |> itemActionError action
        match existing with
        | None ->
            do! fs.Move item.Type item.Path newPath |> itemActionError action
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
        else fs.GetItem oldItem.Path |> itemActionError action
    match existing with
    | None ->
        do! fs.Move oldItem.Type currentPath oldItem.Path |> itemActionError action
        return
            model
            |> MainModel.mapHistory (History.withPathReplaced item.Path oldItem.Path)
            |> performedUndo undoIter (RenamedItem (oldItem, currentName))
            |> ignoreError (Nav.openPath fs parentPath (CursorToPath (oldItem.Path, false)))
    | Some existingItem ->
        return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", oldItem.Type, oldItem.Name, existingItem.IsHidden)
}

let private getDuplicateName (itemRefs: ItemRef seq) =
    itemRefs
    |> Seq.map (fun i -> i.Path.Name)
    |> Seq.groupBy id
    |> Seq.filter (fun (_, items) -> items |> Seq.truncate 2 |> Seq.length > 1)
    |> Seq.map fst
    |> Seq.tryHead

let registerSelectedItems putType (model: MainModel) =
    let itemsToRegister =
        model.ActionItems
        |> Seq.filter (fun item -> item.Type.CanModify)
        |> Seq.map (fun item -> item.Ref)
        |> Seq.toList
    if itemsToRegister.IsEmpty then
        Ok model
    else
        match getDuplicateName itemsToRegister with
        | Some duplicateName ->
            Error (MainStatus.CannotRegisterMultipleItemsWithSameName duplicateName)
        | None ->
            { model with
                Config = { model.Config with YankRegister = Some (putType, itemsToRegister) }
                Status = None
            } |> Ok

let getCopyName name i =
    let (nameNoExt, ext) = Path.SplitName name
    let number = if i = 0 then "" else string (i+1)
    sprintf "%s copy%s%s" nameNoExt number ext

let rec private enumeratePutItems (fsReader: IFileSystemReader) (cancelToken: CancelToken) deepExistsCheck putType
        (copyNamesToReuse: Map<string, string>) destParent srcRefs =
    let rec iter checkExists (destParent: Path) (srcRefs: ItemRef seq) = asyncSeqResult {
        let! existingNames =
            if checkExists then
                fsReader.GetItems destParent
                |> Result.map (List.map (fun i -> i.Name.ToLower()) >> Set)
                |> Result.mapError (fun e -> (destParent, e))
            else
                Ok (Set [])
        let nameExists name =
            existingNames |> Set.contains (name |> String.toLower)
        for { Path = src; Type = typ } in srcRefs do
            if not cancelToken.IsCancelled then
                let! destName, destExists =
                    if putType = Copy && src.Parent = destParent then
                        copyNamesToReuse
                        |> Map.tryFind src.Name
                        |> Option.map (fun name -> Ok (name, nameExists name))
                        |> Option.defaultWith (fun () ->
                            Seq.init 99 (getCopyName src.Name)
                            |> Seq.tryFind (not << nameExists)
                            |> function
                                | Some name -> Ok (name, false)
                                | None -> Error (src, TooManyCopiesOfNameException src.Name :> exn)
                        )
                    else
                        let name = src.Name + if putType = Shortcut then ".lnk" else ""
                        Ok (name, nameExists name)
                let dest = destParent.Join destName
                let putItem = { ItemType = typ; Source = src; Dest = dest; DestExists = destExists }
                match typ with
                | Folder when (putType <> Shortcut && destExists && deepExistsCheck)
                        || not destExists && (putType = Copy || putItem.AreBasePathsDifferent) ->
                    let! items = fsReader.GetItems src |> Result.mapError (fun e -> (src, e))
                    yield putItem
                    if not items.IsEmpty then
                        yield! iter destExists dest (items |> Seq.map (fun i -> i.Ref))
                | Folder | File ->
                    yield putItem
                | _ -> ()
    }
    runSeqAsync (iter true destParent srcRefs)

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

module private MoveUtil =
    let deleteSourceFolders (fs: IFileSystem) paths =
        let rec iter paths = seq {
            for path in paths do
                match fs.GetItems path with
                | Ok items ->
                    let (folders, files) = items |> List.partition (fun item -> item.Type = Folder)
                    yield! folders |> Seq.map (fun folder -> folder.Path) |> iter
                    if files |> List.isEmpty then
                        yield
                            fs.Delete Folder path
                            |> Result.map (cnst path)
                            |> Result.mapError (fun ex -> (path, ex))
                    else
                        yield Error (path, FolderNotEmptyException() :> exn)
                | Error ex ->
                    yield Error (path, ex)
        }
        runAsync (fun () -> iter paths |> Result.partition)

    let getPathReplacements isUndo isCompleted intent succeeded unsuccessful (enumErrorPaths: Path list) =
        let wasCopiedBack overwritten = isUndo && overwritten
        let shouldReplaceAtIntent = isCompleted && not (wasCopiedBack intent.Overwrite)
        let isIncompleteEnumeratedFolder putItem =
            putItem.ItemType = Folder
            && (putItem.DestExists || putItem.AreBasePathsDifferent)
            && (unsuccessful |> List.exists (fun pi -> pi.Source.IsWithin putItem.Source)
                || enumErrorPaths |> List.exists (fun path -> path.IsWithin putItem.Source || path.IsWithin putItem.Dest)
            )
        let putItemAtIntent putItem =
            let rec upToIntent (src: Path) (dest: Path) =
                if dest.Parent = intent.DestParent then (src, dest)
                else upToIntent src.Parent dest.Parent
            let src, dest = upToIntent putItem.Source putItem.Dest
            if src = putItem.Source
            then putItem
            else { putItem with Source = src; Dest = dest }
        succeeded
        |> List.toSeq
        |> applyIf (not shouldReplaceAtIntent) (Seq.filter (fun putItem ->
            not (wasCopiedBack putItem.DestExists) && not (isIncompleteEnumeratedFolder putItem)
        ))
        |> Seq.fold (fun replacements putItem ->
            match replacements with
            | prev :: _ when putItem.Source.IsWithin prev.Source ->
                replacements // skip paths within the previous path
            | _ ->
                let replacement = putItem |> applyIf shouldReplaceAtIntent putItemAtIntent
                replacement :: replacements
        ) []
        |> Seq.map (fun putItem ->
            if isUndo
            then putItem.Dest, putItem.Source
            else putItem.Source, putItem.Dest
        )
        |> Map

let private performPut (fs: IFileSystem) progress undoIter enumErrors putType intent putItems (model: MainModel) =
    asyncSeqResult {
        let isUndo = undoIter |> Option.isSome
        let! results = putItems |> performPutItems fs progress model.CancelToken isUndo putType
        let succeeded, putErrors = results |> Result.partition
        let errorPaths = enumErrors @ putErrors
        let enumeratedItemCount = putItems.Length + enumErrors.Length

        // if nothing succeeded, return error
        if succeeded |> List.isEmpty then
            return MainStatus.PutError (isUndo, putType, errorPaths, enumeratedItemCount)

        let! deleteFolderErrors = async {
            // for folders that were enumerated and their contents were moved, delete source folders
            if putType = Move then
                let isSubPathOf ancestor (path: Path) =
                    path <> ancestor && path.IsWithin ancestor
                let hasSuccessfulMove path =
                    let successPutItems =
                        succeeded |> Seq.filter (fun putItem -> putItem.Source |> isSubPathOf path) |> Seq.cache
                    not (successPutItems |> Seq.isEmpty)
                    && (not isUndo || successPutItems |> Seq.forall (fun putItem -> not putItem.DestExists))
                return!
                    intent.Sources
                    |> Seq.filter (fun intentSource ->
                        intentSource.Type = Folder && hasSuccessfulMove intentSource.Path
                    )
                    |> Seq.map (fun itemRef ->
                        if isUndo
                        then intent.DestParent.Join itemRef.Path.Name
                        else itemRef.Path
                    )
                    |> MoveUtil.deleteSourceFolders fs
                    |> Async.map snd
            else
                return []
        }

        let action = PutItems (putType, intent, succeeded, model.CancelToken.IsCancelled)
        let unsuccessful =
            if not (errorPaths.IsEmpty) || model.CancelToken.IsCancelled then
                putItems |> List.except succeeded
            else
                []

        let updateUndoRedo (model: MainModel) =
            let cancelledAction =
                if model.CancelToken.IsCancelled then
                    Some (PutItems (putType, intent, unsuccessful, true))
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
                let isCompleted = errorPaths.IsEmpty && not model.CancelToken.IsCancelled
                let pathReplacements =
                    MoveUtil.getPathReplacements isUndo isCompleted intent succeeded unsuccessful (enumErrors |> List.map fst)
                model |> MainModel.mapHistory (History.withPathsReplaced pathReplacements)
            else
                model
        let openDest model =
            let destPaths =
                if isUndo then
                    intent.Sources |> List.map (fun itemRef -> itemRef.Path)
                else
                    intent.FilterPutItemsToDestParent succeeded
                    |> Seq.map (fun putItem -> putItem.Dest)
                    |> Seq.toList
                    |> Option.ofCond (not << List.isEmpty)
                    |> Option.defaultWith (fun () ->
                        let rec childOfDestParent (path: Path) =
                            if path.Parent = intent.DestParent then path
                            else childOfDestParent path.Parent
                        [childOfDestParent succeeded.Head.Dest]
                    )
            model |> ignoreError (Nav.openPath fs destPaths.Head.Parent (CursorToAndSelectPaths (destPaths, false)))
        let status =
            // for partial success, set error message instead of returning Error so the caller flow is not short-circuited
            if not errorPaths.IsEmpty then
                MainStatus.Error (MainStatus.PutError (isUndo, putType, errorPaths, enumeratedItemCount))
            else if model.CancelToken.IsCancelled then
                MainStatus.Message (MainStatus.CancelledPut (putType, isUndo, succeeded.Length, putItems.Length))
            else
                deleteFolderErrors
                |> List.tryHead
                |> Option.map (fun (path, ex) ->
                    MainStatus.Error (MainStatus.CouldNotDeleteMoveSource (path.Name, ex))
                )
                |> Option.defaultWith (fun () ->
                    match undoIter with
                    | Some iter ->
                        MainStatus.Message (MainStatus.UndoAction (action, iter, model.RepeatCount))
                    | None ->
                        MainStatus.Message (MainStatus.ActionComplete action)
                )
        yield
            model
            |> updateUndoRedo
            |> updateHistory
            |> MainModel.withStatus status
            |> openDest
    }

let putToDestination (fs: IFileSystem) (progress: Progress) isRedo putType intent model = asyncSeqResult {
    let model = { model with CancelToken = CancelToken() }
    yield model |> MainModel.withBusy (MainStatus.PreparingPut (putType, intent.Sources))
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
    let copyNamesToReuse =
        alreadyPutItemsForResume
        |> intent.FilterPutItemsToDestParent
        |> Seq.map (fun putItem -> putItem.Source.Name, putItem.Dest.Name)
        |> Map
    let! enumerationResults =
        let deepExistsCheck = intent.Overwrite || not alreadyPutItemsForResume.IsEmpty
        enumeratePutItems fs model.CancelToken deepExistsCheck putType copyNamesToReuse intent.DestParent intent.Sources
        |> AsyncSeq.toListAsync
    let putItemsToConfirmOverwrite =
        if not intent.Overwrite && not isRedo then
            enumerationResults |> List.choose (function
                | Ok putItem when putItem.DestExists -> Some putItem
                | _ -> None
            )
        else
            []
    if not putItemsToConfirmOverwrite.IsEmpty then
        let sourceItems =
            putItemsToConfirmOverwrite
            |> Seq.map (fun putItem -> putItem.Source.Parent)
            |> Seq.distinct
            |> Seq.collect (fs.GetItems >> Result.defaultValue [])
            |> Seq.toList
        let destItems = fs.GetItems intent.DestParent |> Result.defaultValue []
        let tryFind itemList path =
            itemList |> List.tryFind (fun item -> item.Path = path)
        let srcExistingPairs, putItemsNotFound =
            putItemsToConfirmOverwrite
            |> List.map (fun putItem ->
                match tryFind sourceItems putItem.Source, tryFind destItems putItem.Dest with
                | Some src, Some existing -> Ok (src, existing)
                | _ -> Error putItem
            )
            |> Result.partition
        if not putItemsNotFound.IsEmpty then
            return MainStatus.CouldNotReadItemsForOverwritePrompt
        // refresh item list to make sure we can see the existing file
        let existingPaths = srcExistingPairs |> List.map (fun (_, item) -> item.Path)
        let! model = Nav.openPath fs model.Location (CursorToAndSelectPaths (existingPaths, true)) model
        yield
            { model with
                InputMode = Some (Confirm (Overwrite (putType, srcExistingPairs)))
                InputText = ""
            }
    else if model.CancelToken.IsCancelled then
        yield model |> MainModel.withMessage (MainStatus.CancelledPut (putType, false, 0, enumerationResults.Length))
    else
        // if resuming cancelled put, skip items already done
        let filterItemsToSkip =
            alreadyPutItemsForResume
            |> List.map (fun putItem -> putItem.Source)
            |> function
                | [] -> id
                | pathsToSkip ->
                    List.filter (fun res ->
                        let path = res |> Result.toOption |> Option.map (fun putItem -> putItem.Source)
                        not (path |> Option.exists (Seq.containedIn pathsToSkip))
            )
        let blockRedoIfDestExists putResult =
            putResult |> Result.bind (fun putItem ->
                if putItem.DestExists
                then Error (putItem.Source, RedoPutBlockedByExistingItemException() :> exn)
                else Ok putItem
            )
        let putItems, enumErrors =
            enumerationResults
            |> filterItemsToSkip
            |> applyIf (isRedo && not intent.Overwrite) (List.map blockRedoIfDestExists)
            |> Result.partition
        if putType = Move || putType = Copy then
            yield model |> MainModel.withBusy (MainStatus.PuttingItem ((putType = Copy), isRedo, intent))
        yield! performPut fs progress None enumErrors putType intent putItems model
}

let putInLocation (fs: IFileSystem) progress isRedo overwrite putType (itemRefs: ItemRef list) (model: MainModel) = asyncSeqResult {
    if model.IsSearchingSubFolders then
        return MainStatus.CannotPutHere
    do! fs.GetItem model.Location
        |> actionError "read put location"
        |> Result.okIf (Option.exists (fun l -> l.Type.CanCreateIn)) MainStatus.CannotPutHere
        |> Result.map ignore
    let itemRefs = itemRefs |> applyIf (putType = Move) (List.filter (fun itemRef -> itemRef.Path.Parent <> model.Location))
    if itemRefs.IsEmpty then
        return MainStatus.CannotMoveToSameFolder
    match getDuplicateName itemRefs with
    | Some duplicateName ->
        return MainStatus.CannotPutMultipleItemsWithSameName (putType, duplicateName)
    | None ->
        let intent = { Sources = itemRefs; DestParent = model.Location; Overwrite = overwrite }
        yield! putToDestination fs progress isRedo putType intent model
}

let put (fs: IFileSystem) progress overwrite (model: MainModel) = asyncSeqResult {
    match model.Config.YankRegister with
    | None -> ()
    | Some (putType, itemRefs) ->
        let! model = putInLocation fs progress false overwrite putType itemRefs model
        let wasImmediatelyCanceled =
            match model.Status with Some (MainStatus.Message (MainStatus.CancelledPut (_, _, 0, _))) -> true | _ -> false
        // if not cancelled or opened input for confirmation, clear yank register
        if not wasImmediatelyCanceled && model.InputMode.IsNone then
            yield { model with Config = { model.Config with YankRegister = None } }
}

let undoMove (fs: IFileSystem) progress undoIter intent (moved: PutItem list) (model: MainModel) =
    asyncSeqResult {
        let model = { model with CancelToken = CancelToken() }
        yield model |> MainModel.withBusy (MainStatus.UndoingPut (false, intent))
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
    yield model |> MainModel.withBusy (MainStatus.UndoingPut (true, intent))
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
            MainStatus.Message (MainStatus.UndoAction (action, undoIter, model.RepeatCount))
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
    do! fs.Delete File shortcutPath |> itemActionError action
    return
        model
        |> MainModel.mapHistory (History.withoutPaths [shortcutPath])
        |> performedUndo undoIter oldAction
        |> applyIf (model.Location = shortcutPath.Parent) (ignoreError (Nav.refresh fs))
}

let clipCopy (os: IOperatingSystem) (model: MainModel) = result {
    let paths =
        model.ActionItems
        |> Seq.filter (fun item -> item.Type <> Empty)
        |> Seq.map (fun i -> i.Path)
        |> Seq.toList
    if paths.IsEmpty then
        return model
    else
        do! os.CopyToClipboard paths |> actionError "copy to clipboard"
        return model |> MainModel.withMessage (MainStatus.ClipboardCopy paths)
}

let private enumerateDeleteItems (fsReader: IFileSystemReader) (cancelToken: CancelToken) (items: Item seq) =
    let rec iter (items: Item seq) = asyncSeq {
        for item in items do
            if not cancelToken.IsCancelled then
                match item.Type with
                | Folder ->
                    match fsReader.GetItems item.Path with
                    | Ok subItems when not subItems.IsEmpty ->
                        yield! iter subItems
                    | _ -> ()
                    yield item
                | File ->
                    yield item
                | _ -> ()
    }
    runSeqAsync (iter items)

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
    let actualDeletedPaths = actualDeleted |> Seq.map (fun i -> i.Path) |> Set
    let itemsDeleted = items |> List.filter (fun i -> actualDeletedPaths |> Set.contains i.Path)

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
            MainStatus.Message (MainStatus.ActionComplete (DeletedItems (permanent, items, false)))

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
            | Some (MainStatus.Message (MainStatus.ActionComplete action)) ->
                let status = MainStatus.Message (MainStatus.RedoAction (action, iter, model.RepeatCount))
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

let private getDropInPutType (event: DragInEvent) (location: Path) (path: Path) =
    let desiredPutType =
        event.PutType |> Option.defaultWith (fun () -> if path.Base = location.Base then Move else Copy)
    event.AllowedPutTypes
    |> List.tryFind ((=) desiredPutType)
    |> Option.orElse (event.AllowedPutTypes |> List.tryHead)

let updateDropInPutType (paths: Path list) (event: DragInEvent) (model: MainModel) =
    event.PutType <- paths |> List.tryHead |> Option.bind (getDropInPutType event model.Location)
    model

let dropIn (fs: IFileSystem) progress paths (event: DragInEvent) (model: MainModel) = asyncSeqResult {
    match getDropInPutType event model.Location (paths |> List.head) with
    | Some putType ->
        match paths with
        | [path] when path.Parent = model.Location -> ()
        | [path] ->
            let! item =
                fs.GetItem path
                |> Result.bind (Result.ofOption (path.Format model.PathFormat |> sprintf "Path not found: %s" |> exn))
                |> actionError "read drop item"
            yield! putInLocation fs progress false false putType [item.Ref] model
        | _ ->
            let parentLists, errors =
                paths
                |> Seq.map (fun p -> p.Parent)
                |> Seq.distinct
                |> Seq.map (fun parent -> fs.GetItems parent |> actionError "read parent of drop item")
                |> Result.partition
            match errors with
            | error :: _ ->
                return error
            | [] ->
                let parentItemRefs = parentLists |> Seq.collect (Seq.map (fun item -> (item.Path, item.Ref))) |> Map
                let itemRefs = paths |> List.choose (fun p -> parentItemRefs |> Map.tryFind p)
                yield! putInLocation fs progress false false putType itemRefs model
    | None -> ()
}

let dropOut (fsReader: IFileSystemReader) (dragOutEvent: DragOutEvent) (model: MainModel) =
    match dragOutEvent.DoDropOut (model.ActionItems |> Seq.map (fun i -> i.Path)) with
    | Some putType when putType = Move ->
        ignoreError (Nav.refresh fsReader) model
    | _ ->
        model
