module Koffee.ItemActionCommands.Delete

open FSharp.Control
open Acadian.FSharp
open Koffee

let confirmDelete (model: MainModel) =
    if model.ActionItems |> List.exists _.Type.CanModify then
        { model with
            InputMode = Some (Confirm Delete)
            InputText = ""
        }
    else
        model

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
