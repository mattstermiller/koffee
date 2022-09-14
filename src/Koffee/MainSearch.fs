module Koffee.Main.Search

open System.Text.RegularExpressions
open FSharp.Control
open VinylUI
open Acadian.FSharp
open Koffee
open Koffee.Main
open Koffee.Main.Util

let private moveCursorTo next reverse predicate model =
    let rotate offset (list: _ list) = list.[offset..] @ list.[0..(offset-1)]
    let indexed = model.Items |> List.indexed
    let items =
        if reverse then
            indexed |> rotate (model.Cursor + (if next then 0 else 1)) |> List.rev
        else
            indexed |> rotate (model.Cursor + (if next then 1 else 0))
    let cursor =
        items
        |> List.filter (snd >> predicate)
        |> List.skip (model.RepeatCount - 1)
        |> List.tryHead
        |> Option.map fst
    match cursor with
    | Some cursor -> model.WithCursor cursor
    | None -> model

let find prefix model =
    { model with LastFind = Some prefix }
    |> moveCursorTo false false (fun i -> i.Name |> String.startsWithIgnoreCase prefix)

let findNext model =
    match model.LastFind with
    | Some prefix ->
        model.WithStatus (MainStatus.find prefix model.RepeatCount)
        |> moveCursorTo true false (fun i -> i.Name |> String.startsWithIgnoreCase prefix)
    | None -> model

let getFilter showHidden searchInput =
    searchInput.Terms
    |> Option.ofString
    |> Option.bind (fun input ->
        if searchInput.Regex then
            let options = if searchInput.CaseSensitive then RegexOptions.None else RegexOptions.IgnoreCase
            try
                let re = Regex(input, options)
                Some (List.filter (fun item -> re.IsMatch item.Name))
            with :? System.ArgumentException -> None
        else
            Some (filterByTerms false searchInput.CaseSensitive input (fun item -> item.Name))
    )
    |> Option.map (fun filter ->
        if not showHidden then
            List.filter (fun i -> not i.IsHidden) >> filter
        else
            filter
    )

let private enumerateSubDirs (fsReader: IFileSystemReader) (progress: Event<_>) isCancelled
                             (searchExclusions: string list) items = asyncSeq {
    let getDirs =
        List.filter (fun i ->
            i.Type = Folder && not (searchExclusions |> List.exists (String.equalsIgnoreCase i.Name))
        )
    let rec enumerate progressFactor dirs = asyncSeq {
        for dir in dirs do
            if not <| isCancelled () then
                let! subItemsRes = runAsync (fun () -> fsReader.GetItems dir.Path)
                if not <| isCancelled () then
                    let subItems = subItemsRes |> Result.toOption |? []
                    let subDirs = getDirs subItems
                    yield subItems
                    let progressFactor = progressFactor / float (subDirs.Length + 1)
                    progress.Trigger (Some progressFactor)
                    yield! enumerate progressFactor subDirs
    }
    let dirs = getDirs items
    progress.Trigger (Some 0.0)
    yield! enumerate (1.0 / float dirs.Length) dirs
    progress.Trigger None
}

let search fsReader (subDirResults: Event<_>) progress (model: MainModel) = asyncSeq {
    let input = { model.SearchInput with Terms = model.InputText }
    let model = { model with SearchCurrent = Some input }
    match model.InputText |> String.isNotEmpty, getFilter model.Config.ShowHidden input with
    | true, Some filter ->
        let withItems items model =
            { model with
                Items = items |> model.ItemsIfEmpty
                Cursor = 0
            } |> Nav.select (SelectItem (model.SelectedItem, false))
        let items = model.Directory |> filter
        if model.SearchInput.SubFolders then
            match model.SubDirectories with
            | None ->
                let cancelToken = CancelToken()
                yield
                    { model with
                        SubDirectories = Some []
                        SubDirectoryCancel = cancelToken
                        Sort = None
                    } |> withItems items
                do! enumerateSubDirs fsReader progress cancelToken.get_IsCancelled model.Config.SearchExclusions
                                     model.Directory
                    |> AsyncSeq.iter subDirResults.Trigger
            | Some subDirs ->
                let items = items @ filter subDirs |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
                yield model |> withItems items
        else
            model.SubDirectoryCancel.Cancel()
            yield { model with SubDirectories = None } |> withItems items
    | true, None ->
        yield model
    | false, _ ->
        yield model |> Nav.listDirectory (SelectItem (model.SelectedItem, false))
}

let addSubDirResults newItems model =
    let filter = model.SearchCurrent |> Option.bind (getFilter model.Config.ShowHidden) |? cnst []
    let items =
        match filter newItems, model.Items with
        | [], _ -> model.Items
        | matches, [item] when item.Type = Empty -> matches
        | matches, _ -> model.Items @ matches
    { model with
        SubDirectories = model.SubDirectories |> Option.map (flip (@) newItems)
        Items = items
    }

let clearSearch (model: MainModel) =
    { model with ShowHistoryType = None }
    |> clearSearchProps
    |> Nav.listDirectory (SelectItem (model.SelectedItem, false))

let refreshOrResearch fsReader subDirResults progress model = asyncSeqResult {
    match model.SearchCurrent with
    | Some current ->
        let selectItem = SelectItem (model.SelectedItem, false)
        let! newModel = model |> Nav.openPath fsReader model.Location selectItem
        let searchModels =
            { newModel with
                InputText = current.Terms
                SearchInput = current
                SearchHistoryIndex = model.SearchHistoryIndex
            }
            |> search fsReader subDirResults progress
            |> AsyncSeq.cache
        yield! searchModels |> AsyncSeq.map Ok
        // when done searching, re-sort
        match! searchModels |> AsyncSeq.tryLast with
        | Some newModel ->
            let items = newModel.Items |> (newModel.Sort |> Option.map SortField.SortByTypeThen |? id)
            yield { newModel with Items = items } |> Nav.select selectItem
        | None -> ()
    | None ->
        yield! Nav.refresh fsReader model
}
