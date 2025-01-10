namespace Koffee.ItemActionCommands

open VinylUI
open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.ItemActionCommands

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
                else Ok model
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
        | ClearYank -> Sync (fun m -> { m with Config = { m.Config with YankRegister = None } })
        | Put -> AsyncResult (Put.put fs progress false)
        | Recycle -> AsyncResult (fun m -> Delete.recycle fs progress m.ActionItems m)
        | ConfirmDelete -> Sync Delete.confirmDelete
        | ClipboardCut -> SyncResult (Put.yankToClipboard false os)
        | ClipboardCopy -> SyncResult (Put.yankToClipboard true os)
        | ClipboardCopyPaths -> SyncResult (Put.copyPathsToClipboard os)
        | ClipboardPaste -> AsyncResult (Put.clipboardPaste fs os progress)
        | Undo -> AsyncResult (Undo.undo fs progress)
        | Redo -> AsyncResult (Undo.redo fs progress)
