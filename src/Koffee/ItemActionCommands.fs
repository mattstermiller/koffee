namespace Koffee.ItemActionCommands

open VinylUI
open FSharp.Control
open Acadian.FSharp
open Koffee

module Attributes =
    let private performSetHidden (fs: IFileSystem) (progress: Progress) (cancelToken: CancelToken) hide (items: Item list) =
        let incrementProgress = progress.GetIncrementer items.Length
        progress.Start ()
        runAsync (fun () ->
            items
            |> Seq.takeWhile (fun _ -> not cancelToken.IsCancelled)
            |> Seq.map (fun item ->
                fs.SetHidden hide item.Type item.Path
                |> Result.map (fun () -> item)
                |> Result.mapError (fun e -> (item.Path, e))
                |>! incrementProgress
            )
            |> Seq.toList
            |>! progress.Finish
        )

    let setHidden fs progress undoIter hide (items: Item list) (model: MainModel) = asyncSeqResult {
        let isUndo = undoIter |> Option.isSome
        let doHide = if isUndo then not hide else hide
        let model = { model with CancelToken = CancelToken() }
        yield model |> MainModel.withBusy (MainStatus.TogglingHidden (doHide, items))
        let! results = performSetHidden fs progress model.CancelToken doHide items

        let succeeded, errors = results |> Result.partition

        // if nothing succeeded, return error
        let errorStatus = lazy MainStatus.ToggleHiddenError (doHide, errors, items.Length)
        if succeeded |> List.isEmpty then
            return errorStatus.Value

        let action = ToggleHidden (hide, succeeded, model.CancelToken.IsCancelled)
        let unsuccessful =
            if not (errors.IsEmpty) || model.CancelToken.IsCancelled
            then items |> List.except succeeded
            else []

        let updateUndoRedo (model: MainModel) =
            let cancelledAction =
                if model.CancelToken.IsCancelled
                then Some (ToggleHidden (hide, unsuccessful, true))
                else None
            if isUndo then
                model
                |> Option.foldBack MainModel.pushUndo cancelledAction
                |> MainModel.pushRedo action
            else
                model
                |> MainModel.pushUndo action
                |> MainModel.withRedoStack (cancelledAction |> Option.toList)

        let updateList model =
            let cursorMove =
                if doHide && not model.Config.ShowHidden && errors.IsEmpty
                then model.KeepCursorByPath
                else CursorToAndSelectPaths (items |> List.map _.Path, false)
            let location = items.Head.Path.Parent
            if location = model.Location then
                let updatedPaths = succeeded |> Seq.map _.Path |> Set
                let directory =
                    model.Directory
                    |> List.map (fun item ->
                        if updatedPaths.Contains item.Path
                        then { item with IsHidden = doHide }
                        else item
                    )
                { model with Directory = directory }
                |> NavigationCommands.listDirectory cursorMove
            else
                NavigationCommands.openPathIgnoreError fs location cursorMove model

        let status =
            // for partial success, set error message instead of returning Error so the caller flow is not short-circuited
            if not errors.IsEmpty then
                MainStatus.Error errorStatus.Value
            else if model.CancelToken.IsCancelled then
                MainStatus.Message (MainStatus.CancelledToggleHidden (hide, isUndo, succeeded.Length, items.Length))
            else
                match undoIter with
                | Some iter ->
                    MainStatus.Message (MainStatus.UndoAction (action, iter, model.RepeatCount))
                | None ->
                    MainStatus.Message (MainStatus.ActionComplete action)

        yield
            model
            |> updateUndoRedo
            |> MainModel.withStatus status
            |> updateList
    }

    let toggleHidden fs progress (model: MainModel) =
        if model.ActionItems.IsEmpty then
            AsyncSeq.empty
        else
            let hide = not (model.ActionItems |> List.forall _.IsHidden)
            setHidden fs progress None hide model.ActionItems model

module Undo =
    let rec private undoIter iter fs progress model = asyncSeqResult {
        match model.UndoStack with
        | action :: rest ->
            let model = { model with UndoStack = rest }
            yield model
            let! model =
                match action with
                | CreatedItem item ->
                    Create.undoCreate fs iter item model
                | RenamedItem (oldItem, curName) ->
                    Rename.undoRename fs iter oldItem curName model
                    |> AsyncSeq.singleton
                | PutItems (Move, intent, actual, _) ->
                    Put.undoMove fs progress iter intent actual model
                | PutItems (Copy, intent, actual, _) ->
                    Put.undoCopy fs progress iter intent actual model
                | PutItems (Shortcut, intent, actual, _) ->
                    Put.undoShortcut fs iter action actual.Head.Dest model
                    |> AsyncSeq.singleton
                | DeletedItems (permanent, items, _) ->
                    Error (MainStatus.CannotUndoDelete (permanent, items))
                    |> AsyncSeq.singleton
                | ToggleHidden (hide, items, _) ->
                    Attributes.setHidden fs progress (Some iter) hide items model
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
                then NavigationCommands.openPath fs path cursorMove model
                else Ok (NavigationCommands.moveCursor cursorMove model)
            let! model = asyncSeqResult {
                match action with
                | CreatedItem item ->
                    let! model = openPath item.Path.Parent CursorStay
                    yield! Create.create fs item.Type item.Name model
                | RenamedItem (item, newName) ->
                    let! model = openPath item.Path.Parent CursorStay
                    yield! Rename.rename fs item newName model
                | PutItems (putType, intent, _, _) ->
                    let! model = openPath intent.DestParent CursorStay
                    yield! Put.putToDestination fs progress true putType intent model
                | DeletedItems (permanent, items, _) ->
                    // normally, redo of delete is impossible because undo is impossible, but cancellation pushes redo action for resuming
                    let cursor = CursorToAndSelectPaths (items |> List.map (fun i -> i.Path), true)
                    let! model = openPath items.Head.Path.Parent cursor
                    yield model |> MainModel.withBusy (MainStatus.RedoingDeleting (permanent, items))
                    let deleteFunc = if permanent then Delete.delete else Delete.recycle
                    yield! deleteFunc fs progress items model
                | ToggleHidden (hide, items, _) ->
                    yield! Attributes.setHidden fs progress None hide items model
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

type Handler(fs: IFileSystem, os: IOperatingSystem, progress: Progress) =
    member _.UpdateDropInPutType paths event model = Put.updateDropInPutType paths event model
    member _.DropIn paths event model = Put.dropIn fs progress paths event model
    member _.DropOut event model = Put.dropOut fs event model

    member _.Handle (command: ItemActionCommand) =
        match command with
        | CreateFile -> SyncResult (Create.inputNewFile fs)
        | CreateFolder -> SyncResult (Create.inputNewFolder fs)
        | StartRename part -> Sync (Rename.inputRename part)
        | Yank putType -> SyncResult (Put.yankSelectedItems putType)
        | ClearYank -> Sync (fun m -> { m with MainModel.History.YankRegister = None })
        | Put -> AsyncResult (Put.put fs progress false)
        | Trash -> AsyncResult (fun m -> Delete.recycle fs progress m.ActionItems m)
        | ConfirmDelete -> Sync Delete.confirmDelete
        | ClipboardCut -> SyncResult (Put.yankToClipboard false os)
        | ClipboardCopy -> SyncResult (Put.yankToClipboard true os)
        | ClipboardCopyPaths -> SyncResult (Put.copyPathsToClipboard os)
        | ClipboardPaste -> AsyncResult (Put.clipboardPaste fs os progress)
        | ToggleHiddenAttribute -> AsyncResult (Attributes.toggleHidden fs progress)
        | Undo -> AsyncResult (Undo.undo fs progress)
        | Redo -> AsyncResult (Undo.redo fs progress)
        | ExecuteTool toolName -> SyncResult (Tools.executeTool os fs toolName)

    member _.HandleNewItemInputEvent isFolder (evt: InputEvent) (model: MainModel) = asyncSeq {
        match evt with
        | InputCharTyped (char, keyHandler) ->
            suppressInvalidPathChar char keyHandler
        | InputSubmit ->
            let model = { model with InputMode = None }
            yield model
            let itemType = if isFolder then Folder else File
            yield! model |> handleAsyncResult (Create.create fs itemType model.InputText)
        | _ -> ()
    }

    member _.HandleRenameInputEvent (evt: InputEvent) (model: MainModel) = asyncSeq {
        match evt with
        | InputCharTyped (char, keyHandler) ->
            suppressInvalidPathChar char keyHandler
        | InputSubmit ->
            match model.CursorItem with
            | Some item ->
                yield { model with InputMode = None } |> handleSyncResult (Rename.rename fs item model.InputText)
            | None ->
                yield model
        | _ -> ()
    }

    member _.ConfirmOverwrite isYes (model: MainModel) = asyncSeqResult {
        if isYes then
            yield! Put.put fs progress true model

        else if not model.Config.ShowHidden && model.ActionItems |> List.exists (fun i -> i.IsHidden) then
            // if we were temporarily showing a hidden file, refresh
            yield NavigationCommands.listDirectory CursorStay model
    }

    member _.ConfirmDelete items isYes (model: MainModel) = asyncSeqResult {
        if isYes then
            yield! Delete.delete fs progress items model
    }
