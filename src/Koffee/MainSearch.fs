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
    | Some index -> { model with InputError = None } |> MainModel.withCursor index
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
        model
        |> MainModel.withMessage (MainStatus.Find (prefix, model.RepeatCount))
        |> performFind true false prefix
    | None -> model

let getFilter showHidden searchInput =
    let filterRes =
        if searchInput.Regex then
            let options = if searchInput.CaseSensitive then RegexOptions.None else RegexOptions.IgnoreCase
            match tryResult (fun () -> Regex(searchInput.Terms, options)) with
            | Ok re -> Ok (List.filter (fun item -> re.IsMatch item.Name))
            | Error ex -> Error (InvalidRegex ex.Message)
        else
            match searchInput.Terms |> parseSearchTerms with
            | Some terms -> Ok (filterByTerms false searchInput.CaseSensitive terms (fun item -> item.Name) >> Seq.toList)
            | None -> Ok id
    filterRes |> Result.map (
        applyIf (not showHidden) (fun filter ->
            List.filter (fun i -> not i.IsHidden) >> filter
        )
    )

let private enumerateSubDirs (fsReader: IFileSystemReader) (progress: Progress) isCancelled
                             (searchExclusions: string list) items = asyncSeq {
    let getDirs =
        List.filter (fun i ->
            i.Type = Folder && not (searchExclusions |> List.exists (String.equalsIgnoreCase i.Name))
        )
    let rec enumerate progressFactor dirs = asyncSeq {
        for dir in dirs do
            if not <| isCancelled () then
                let subItems = fsReader.GetItems dir.Path |> Result.defaultValue []
                if not <| isCancelled () then
                    let subDirs = getDirs subItems
                    yield subItems
                    let progressFactor = progressFactor / float (subDirs.Length + 1)
                    progress.Add progressFactor
                    yield! enumerate progressFactor subDirs
    }
    let dirs = getDirs items
    progress.Start ()
    yield! runSeqAsync (enumerate (1.0 / float dirs.Length) dirs)
    progress.Finish ()
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
            } |> Nav.moveCursor model.KeepCursorByPath
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
        yield model |> Nav.listDirectory model.KeepCursorByPath
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
            yield model |> MainModel.withError MainStatus.NoPreviousSearch
}

let traverseSearchHistory offset model =
    let index =
        (model.SearchHistoryIndex |? -1) + offset
        |> min (model.History.Searches.Length-1)
        |> Option.ofCond (flip (>=) 0)
    let search =
        match index with
        | Some index -> model.History.Searches.[index]
        | None -> Search.Default
    { model with
        InputText = search.Terms
        InputTextSelection = (search.Terms.Length, 0)
        SearchInput = search
        SearchHistoryIndex = index
        HistoryDisplay = index |> Option.map (cnst SearchHistory)
    }

let deleteSearchHistory model =
    match model.SearchHistoryIndex with
    | Some index ->
        model
        |> MainModel.mapHistory (History.withoutSearchIndex index)
        |> traverseSearchHistory 0
    | None -> model

let clearSearch (model: MainModel) =
    { model with HistoryDisplay = None }
    |> clearSearchProps
    |> Nav.listDirectory model.KeepCursorByPath

let refreshOrResearch fsReader subDirResults progress model = asyncSeqResult {
    match model.SearchCurrent with
    | Some current ->
        let cursor = model.KeepCursorByPath
        let! newModel = model |> Nav.openPath fsReader model.Location cursor
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
            yield { newModel with Items = items } |> Nav.moveCursor cursor
        | None -> ()
    | None ->
        yield! Nav.refresh fsReader model
}
