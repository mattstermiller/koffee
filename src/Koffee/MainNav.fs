module Koffee.Main.Nav

open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let select selectType (model: MainModel) =
    model |> MainModel.withCursor (
        match selectType with
        | SelectNone -> model.Cursor
        | SelectIndex index -> index
        | SelectName name ->
            let matchFunc =
                if name.Length = 2 && name.[1] = ':'
                then String.startsWithIgnoreCase // if name is drive, use "starts with" because drives have labels
                else String.equalsIgnoreCase
            model.Items |> List.tryFindIndex (fun i -> matchFunc name i.Name) |? model.Cursor
        | SelectItem (item, _) ->
            model.Items |> List.tryFindIndex (fun i -> i.Path = item.Path) |? model.Cursor
    )

let listDirectory selectType model =
    let selectHiddenItem =
        match selectType with
        | SelectItem (item, true) -> Some item
        | _ -> None
    let items =
        model.Directory
        |> List.filter (fun i -> model.Config.ShowHidden || not i.IsHidden || Some i = selectHiddenItem)
        |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
        |> model.ItemsOrEmpty
    { model with Items = items } |> select selectType

let private getDirectory (fsReader: IFileSystemReader) (model: MainModel) path =
    if path = Path.Network then
        model.History.NetHosts
        |> List.map (sprintf @"\\%s")
        |> List.choose Path.Parse
        |> List.map (fun path -> Item.Basic path path.Name NetHost)
        |> Ok
    else
        fsReader.GetItems path
        |> Result.mapError (fun e -> MainStatus.CouldNotOpenPath (path, model.PathFormat, e))

let openPath (fsReader: IFileSystemReader) path select (model: MainModel) = result {
    let! directory = getDirectory fsReader model path
    return
        { model with
            Directory = directory
            History = model.History |> History.withFolderPath model.Config.Limits.PathHistory path
            Sort = Some (model.History.FindSortOrDefault path |> PathSort.toTuple)
        }
        |> MainModel.withPushedLocation path
        |> clearSearchProps
        |> listDirectory select
}

let openUserPath (fsReader: IFileSystemReader) pathStr (model: MainModel) =
    match Path.Parse pathStr with
    | Some path ->
        match fsReader.GetItem path with
        | Ok (Some item) when item.Type = File ->
            model |> MainModel.clearStatus |> openPath fsReader path.Parent (SelectItem (item, true))
        | Ok _ ->
            model |> MainModel.clearStatus |> openPath fsReader path SelectNone
        | Error e ->
            Error <| MainStatus.ActionError ("open path", e)
    | None ->
        Error <| MainStatus.InvalidPath pathStr

let openInputPath fsReader os pathStr (evtHandler: EvtHandler) model = result {
    let pathStr = OsUtility.subEnvVars os pathStr
    let! model = openUserPath fsReader pathStr model
    evtHandler.Handle ()
    return model
}

let openSelected fsReader (os: IOperatingSystem) fnAfterOpen (model: MainModel) =
    let item = model.SelectedItem
    match item.Type with
    | Folder | Drive | NetHost | NetShare ->
        model |> MainModel.clearStatus |> openPath fsReader item.Path SelectNone
    | File ->
        let mapOpenError res = res |> Result.mapError (fun e -> MainStatus.CouldNotOpenFile (item.Name, e))
        let shortcutFolder = result {
            let! targetPath =
                if item.Path.Extension |> String.equalsIgnoreCase "lnk" then
                    fsReader.GetShortcutTarget item.Path |> mapOpenError
                    |> Result.map Path.Parse
                else
                    Ok None
            match targetPath with
            | Some targetPath ->
                let! target =
                    fsReader.GetItem targetPath
                    |> mapOpenError
                    |> Result.bind (Result.ofOption (MainStatus.ShortcutTargetMissing (targetPath.Format model.PathFormat)))
                return if target.Type = Folder then Some target.Path else None
            | None ->
                return None
        }
        shortcutFolder
        |> Result.bind (function
            | Some shortcutFolder ->
                model |> MainModel.clearStatus |> openPath fsReader shortcutFolder SelectNone
            | None ->
                os.OpenFile item.Path
                |> mapOpenError
                |> Result.map (fun () ->
                    fnAfterOpen |> Option.iter (fun f -> f())
                    model
                    |> MainModel.mapHistory (History.withFilePath model.Config.Limits.PathHistory item.Path)
                    |> MainModel.withMessage (MainStatus.OpenFile item.Name)
                )
        )
    | Empty -> Ok model

let openParent fsReader (model: MainModel) =
    if model.SearchCurrent.IsSome then
        if model.SelectedItem.Type = Empty then
            Ok (model |> clearSearchProps |> listDirectory SelectNone)
        else
            model
            |> MainModel.clearStatus
            |> openPath fsReader model.SelectedItem.Path.Parent (SelectItem (model.SelectedItem, false))
    else
        let rec getParent n (path: Path) (currentName: string) =
            if n < 1 || path = Path.Root then (path, currentName)
            else getParent (n-1) path.Parent path.Name
        let path, selectName = getParent model.RepeatCount model.Location model.Location.Name
        model |> MainModel.clearStatus |> openPath fsReader path (SelectName selectName)

let refresh fsReader (model: MainModel) =
    openPath fsReader model.Location (SelectItem (model.SelectedItem, false)) model

let refreshDirectory fsReader (model: MainModel) =
    getDirectory fsReader model model.Location
    |> function
        | Ok dir -> { model with Directory = dir }
        | _ -> model

let rec private shiftStacks n current fromStack toStack =
    match fromStack with
    | newCurrent :: fromStack when n > 0 ->
        shiftStacks (n-1) newCurrent fromStack (current :: toStack)
    | _ ->
        (current, fromStack, toStack)

let private removeIndexOrLast index list =
    let index = min index (List.length list - 1)
    list |> Seq.indexed |> Seq.filter (fst >> (<>) index) |> Seq.map snd |> Seq.toList

let back fsReader model =
    if model.BackStack = [] then
        model
    else
        let (path, cursor), fromStack, toStack =
            shiftStacks model.RepeatCount (model.Location, model.Cursor) model.BackStack model.ForwardStack
        model
        |> MainModel.clearStatus
        |> openPath fsReader path (SelectIndex cursor)
        |> function
            | Ok model ->
                { model with BackStack = fromStack; ForwardStack = toStack }
            | Error e ->
                { model with BackStack = model.BackStack |> removeIndexOrLast (model.RepeatCount-1) }
                |> MainModel.withError e

let forward fsReader model =
    if model.ForwardStack = [] then
        model
    else
        let (path, cursor), fromStack, toStack =
            shiftStacks model.RepeatCount (model.Location, model.Cursor) model.ForwardStack model.BackStack
        model
        |> MainModel.clearStatus
        |> openPath fsReader path (SelectIndex cursor)
        |> function
            | Ok model -> { model with BackStack = toStack; ForwardStack = fromStack }
            | Error e ->
                { model with ForwardStack = model.ForwardStack |> removeIndexOrLast (model.RepeatCount-1) }
                |> MainModel.withError e

let sortList field model =
    let desc =
        match model.Sort with
        | Some (f, desc) when f = field -> not desc
        | _ -> field = Modified
    let selectType = if model.Cursor = 0 then SelectNone else SelectItem (model.SelectedItem, false)
    { model with
        Sort = Some (field, desc)
        Items = model.Items |> SortField.SortByTypeThen (field, desc)
        History = model.History |> History.withPathSort model.Location { Sort = field; Descending = desc }
    }
    |> MainModel.withMessage (MainStatus.Sort (field, desc))
    |> select selectType

let suggestPaths (fsReader: IFileSystemReader) (model: MainModel) = asyncSeq {
    let getSuggestions search (paths: HistoryPath list) =
        paths |> filterByTerms true false search (fun p -> p.PathValue.Name)
    let dirAndSearch =
        model.LocationInput
        |> String.replace @"\" "/"
        |> String.lastIndexOf "/"
        |> Option.ofCond (flip (>=) 0)
        |> Option.map (fun i ->
            let dir = model.LocationInput |> String.substring 0 i
            let search = model.LocationInput |> String.substringFrom (i + 1)
            (dir, search)
        )
    match dirAndSearch with
    | Some (dir, search) ->
        match dir |> Path.Parse with
        | Some dir ->
            let! pathsRes =
                match model.PathSuggestCache with
                | Some (cachePath, cache) when cachePath = dir -> async { return cache }
                | _ -> runAsync (fun () ->
                    fsReader.GetFolders dir
                    |> Result.map (List.map (fun i -> i.Path))
                    |> Result.mapError (fun e -> e.Message)
                )
            let toHistory p = { PathValue = p; IsDirectory = true }
            let suggestions = pathsRes |> Result.map (List.map toHistory >> getSuggestions search)
            yield { model with PathSuggestions = suggestions; PathSuggestCache = Some (dir, pathsRes) }
        | None ->
            yield { model with PathSuggestions = Ok [] }
    | None when model.LocationInput |> String.isNotEmpty ->
        let suggestions = getSuggestions model.LocationInput model.History.Paths
        yield { model with PathSuggestions = Ok suggestions }
    | None ->
        yield { model with PathSuggestions = Ok [] }
}

let deletePathSuggestion (path: HistoryPath) (model: MainModel) =
    // if location input does not contain a slash, the suggestions are from history
    if not (model.LocationInput |> String.contains "/" || model.LocationInput |> String.contains @"\") then
        { model with
            History = { model.History with Paths = model.History.Paths |> List.filter ((<>) path) }
            PathSuggestions = model.PathSuggestions |> Result.map (List.filter ((<>) path))
        }
    else model

let scrollView (gridScroller: DataGridScroller) scrollType (model: MainModel) =
    let topIndex =
        match scrollType with
        | CursorTop -> model.Cursor
        | CursorMiddle -> model.Cursor - (model.PageSize/2)
        | CursorBottom -> model.Cursor - model.PageSize + 1
    // unfortunately, a two-way binding on scroll index is not feasible because the ScrollViewer does not exist at
    // binding time, so we're using a side effect :(
    gridScroller.ScrollTo topIndex
    model
