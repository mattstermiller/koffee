module Koffee.ItemActionCommands.Create

open Acadian.FSharp
open Koffee

let private inputNewItem (inputType: InputType) (fsReader: IFileSystemReader) (model: MainModel) = result {
    if model.IsSearchingSubFolders then
        return! Error MainStatus.CannotPutHere
    do!
        match fsReader.GetItem model.Location with
        | Ok (Some item) when item.Type.CanCreateIn ->
            Ok ()
        | Ok _ ->
            Error MainStatus.CannotPutHere
        | Error e ->
            Error (MainStatus.ActionError ("create item", e))
    return
        { model with
            InputMode = Some (Input inputType)
            InputText = ""
        }
}

let inputNewFile fs = inputNewItem NewFile fs
let inputNewFolder fs = inputNewItem NewFolder fs

let create (fs: IFileSystem) itemType name model = asyncSeqResult {
    let itemPath = model.Location.Join name
    let action = CreatedItem (Item.Basic itemPath name itemType)
    let! existing = fs.GetItem itemPath |> itemActionError action
    match existing with
    | None ->
        do! fs.Create itemType itemPath |> itemActionError action
        let model = model |> performedAction action
        yield model
        yield! NavigationCommands.openPath fs model.Location (CursorToPath (itemPath, false)) model
    | Some existing ->
        yield! NavigationCommands.openPath fs model.Location (CursorToPath (itemPath, true)) model
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
        |> applyIf (model.Location = item.Path.Parent) (NavigationCommands.refreshIgnoreError fs)
}
