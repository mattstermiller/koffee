module Koffee.Main.Search

open System.Text.RegularExpressions
open FSharp.Control
open VinylUI
open Acadian.FSharp
open Koffee
open Koffee.Main
open Koffee.Main.Util

let private performFind next reverse prefix model =
    let rotate offset (list: _ list) = list.[offset..] @ list.[0..(offset-1)]
    let indexed = model.Items |> List.indexed
    let items =
        if reverse then
            indexed |> rotate (model.Cursor + (if next then 0 else 1)) |> List.rev
        else
            indexed |> rotate (model.Cursor + (if next then 1 else 0))
    let index =
        items
        |> List.filter (fun (_, item) -> item.Name |> String.startsWithIgnoreCase prefix)
        |> List.skip (model.RepeatCount - 1)
        |> List.tryHead
        |> Option.map fst
    match index with
    | Some index -> { (model.WithCursor index) with InputError = None }
    | None -> { model with InputError = Some (FindFailure prefix) }

let find model =
    if model.InputText |> String.isNotEmpty then
        { model with LastFind = Some model.InputText }
        |> performFind false false model.InputText
    else
        { model with InputError = None }

let findNext model =
    match model.LastFind with
    | Some prefix ->
        model.WithMessage (MainStatus.Find (prefix, model.RepeatCount))
        |> performFind true false prefix
    | None -> model

let getFilter showHidden searchInput =
    (
        if searchInput.Regex then
            let options = if searchInput.CaseSensitive then RegexOptions.None else RegexOptions.IgnoreCase
            match tryResult (fun () -> Regex(searchInput.Terms, options)) with
            | Ok re -> Ok (List.filter (fun item -> re.IsMatch item.Name))
            | Error ex -> Error (InvalidRegex ex.Message)
        else
            Ok (filterByTerms false searchInput.CaseSensitive searchInput.Terms (fun item -> item.Name))
    ) |> Result.map (fun filter ->
        if not showHidden then
            (List.filter (fun i -> not i.IsHidden) >> filter)
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
                    let subItems = subItemsRes |> Result.defaultValue []
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
    let model = { model with SearchCurrent = Some input; InputError = None }
    let filterResult =
        input
        |> Option.ofCond (fun i -> i.Terms |> String.isNotWhiteSpace)
        |> Option.map (getFilter model.Config.ShowHidden)
    match filterResult with
    | Some (Ok filter) ->
        let withItems items model =
            { model with
                Items =
                    items
                    |> model.ItemsOrEmpty
                    |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
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
                        CancelToken = cancelToken
                        Sort = None
                    } |> withItems items
                let exclusions = model.Config.SearchExclusions |> List.filter (not << String.startsWith "/")
                do! enumerateSubDirs fsReader progress cancelToken.get_IsCancelled exclusions model.Directory
                    |> AsyncSeq.iter subDirResults.Trigger
            | Some subDirs ->
                yield model |> withItems (items @ filter subDirs)
        else
            model.CancelToken.Cancel()
            yield { model with SubDirectories = None } |> withItems items
    | Some (Error e) ->
        yield { model with InputError = Some e }
    | None ->
        yield model |> Nav.listDirectory (SelectItem (model.SelectedItem, false))
}

let addSubDirResults newItems model =
    let filter =
        model.SearchCurrent
        |> Option.bind (getFilter model.Config.ShowHidden >> Result.toOption)
        |? cnst []
    let items =
        match filter newItems, model.Items with
        | [], _ -> model.Items
        | matches, [item] when item.Type = Empty -> matches
        | matches, _ -> model.Items @ matches
    { model with
        SubDirectories = model.SubDirectories |> Option.map (flip (@) newItems)
        Items = items
    }

let repeatSearch fsReader subDirResults progress (model: MainModel) = asyncSeq {
    if model.SearchCurrent.IsNone then
        match model.History.Searches |> List.tryHead with
        | Some prevSearch ->
            yield!
                { model with
                    InputText = prevSearch.Terms
                    SearchInput = prevSearch
                }
                |> search fsReader subDirResults progress
        | None ->
            yield model.WithError MainStatus.NoPreviousSearch
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
