module Koffee.ItemActionCommands.Put

open FSharp.Control
open Acadian.FSharp
open Koffee

let private getDuplicateName (itemRefs: ItemRef seq) =
    itemRefs
    |> Seq.map (fun i -> i.Path.Name)
    |> Seq.groupBy id
    |> Seq.filter (fun (_, items) -> items |> Seq.truncate 2 |> Seq.length > 1)
    |> Seq.map fst
    |> Seq.tryHead

let yankSelectedItems putType (model: MainModel) =
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
                MainModel.History.YankRegister = Some (putType, itemsToRegister)
                Status = None
            } |> Ok

let getCopyName itemType name i =
    let (nameNoExt, ext) =
        if itemType = File
        then Path.SplitName name
        else (name, "")
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
                            Seq.init 99 (getCopyName typ src.Name)
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
    progress.Start ()
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
                    |> Option.ofCond Seq.isNotEmpty
                    |> Option.defaultWith (fun () ->
                        let rec childOfDestParent (path: Path) =
                            if path.Parent = intent.DestParent then path
                            else childOfDestParent path.Parent
                        [childOfDestParent succeeded.Head.Dest]
                    )
            model |> NavigationCommands.openPathIgnoreError fs destPaths.Head.Parent (CursorToAndSelectPaths (destPaths, false))
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
        let! model = NavigationCommands.openPath fs model.Location (CursorToAndSelectPaths (existingPaths, true)) model
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
    match model.History.YankRegister with
    | None -> ()
    | Some (putType, itemRefs) ->
        let! model = putInLocation fs progress false overwrite putType itemRefs model
        let wasImmediatelyCanceled =
            match model.Status with Some (MainStatus.Message (MainStatus.CancelledPut (_, _, 0, _))) -> true | _ -> false
        // if not cancelled or opened input for confirmation, clear yank register
        if not wasImmediatelyCanceled && model.InputMode.IsNone then
            yield { model with MainModel.History.YankRegister = None }
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
        |> applyIf (model.Location = intent.DestParent) (NavigationCommands.refreshIgnoreError fs)
}

let undoShortcut (fs: IFileSystem) undoIter oldAction shortcutPath (model: MainModel) = result {
    let item = Item.Basic shortcutPath shortcutPath.Name File
    let action = DeletedItems (true, [item], false)
    do! fs.Delete File shortcutPath |> itemActionError action
    return
        model
        |> MainModel.mapHistory (History.withoutPaths [shortcutPath])
        |> performedUndo undoIter oldAction
        |> applyIf (model.Location = shortcutPath.Parent) (NavigationCommands.refreshIgnoreError fs)
}

let private getActionItemPaths (model: MainModel) =
    model.ActionItems
    |> Seq.map (fun i -> i.Path)
    |> Seq.toList

let yankToClipboard copy (os: IOperatingSystem) (model: MainModel) = result {
    let paths = getActionItemPaths model
    if paths.IsEmpty then
        return model
    else
        do! os.SetClipboardFileDrop copy paths |> actionError "set clipboard file drop"
        return model |> MainModel.withMessage (MainStatus.ClipboardYank (copy, paths))
}

let copyPathsToClipboard (os: IOperatingSystem) (model: MainModel) = result {
    let paths = getActionItemPaths model
    if paths.IsEmpty then
        return model
    else
        let text = paths |> Seq.map string |> String.concat "\n"
        do! os.SetClipboardText text |> actionError "set clipboard text"
        return model |> MainModel.withMessage (MainStatus.ClipboardCopyPaths paths)
}

let private getItemRefs (fsReader: IFileSystemReader) paths = result {
    match paths with
    | [path] ->
        let! item =
            fsReader.GetItem path
            |> actionError "read item"
            |> Result.bind (Result.ofOption (MainStatus.PathNotFound path))
        return [item.Ref]
    | _ ->
        let parentLists, errors =
            paths
            |> Seq.map (fun p -> p.Parent)
            |> Seq.distinct
            |> Seq.map (fun parent -> fsReader.GetItems parent |> actionError "read parent of item")
            |> Result.partition
        match errors with
        | error :: _ ->
            return! Error error
        | [] ->
            let parentItemRefs = parentLists |> Seq.collect (Seq.map (fun item -> (item.Path, item.Ref))) |> Map
            return paths |> List.choose (fun p -> parentItemRefs |> Map.tryFind p)
}

let clipboardPaste (fs: IFileSystem) (os: IOperatingSystem) progress (model: MainModel) = asyncSeqResult {
    match! os.GetClipboardFileDrop () |> actionError "get from clipboard" with
    | _, [] ->
        yield model |> MainModel.withMessage MainStatus.NoItemsToPaste
    | putType, paths ->
        let! itemRefs = getItemRefs fs paths
        yield! putInLocation fs progress false false putType itemRefs model
}

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
        | _ ->
            let! itemRefs = getItemRefs fs paths
            yield! putInLocation fs progress false false putType itemRefs model
    | None -> ()
}

let dropOut (fsReader: IFileSystemReader) (dragOutEvent: DragOutEvent) (model: MainModel) =
    match dragOutEvent.DoDropOut (getActionItemPaths model) with
    | Some putType when putType = Move ->
        NavigationCommands.refreshIgnoreError fsReader model
    | _ ->
        model
