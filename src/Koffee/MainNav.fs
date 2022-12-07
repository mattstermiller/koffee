module Koffee.Main.Nav

open FSharp.Control
open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let select selectType (model: MainModel) =
    model.WithCursor (
        match selectType with
        | SelectNone -> model.Cursor
        | SelectIndex index -> index
        | SelectName name ->
            model.Items |> List.tryFindIndex (fun i -> String.equalsIgnoreCase i.Name name) |? model.Cursor
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

let openPath (fsReader: IFileSystemReader) path select (model: MainModel) = result {
    let! directory =
        if path = Path.Network then
            model.History.NetHosts
            |> List.map (sprintf @"\\%s")
            |> List.choose Path.Parse
            |> List.map (fun path -> Item.Basic path path.Name NetHost)
            |> Ok
        else
            fsReader.GetItems path |> actionError "open path"
    return
        { model.WithPushedLocation path with
            Directory = directory
            History = model.History.WithPathAndNetHost model.Config.Limits.PathHistory path
            Sort = Some (model.History.FindSortOrDefault path |> PathSort.toTuple)
        }
        |> clearSearchProps
        |> listDirectory select
}

let openUserPath (fsReader: IFileSystemReader) pathStr (model: MainModel) =
    match Path.Parse pathStr with
    | Some path ->
        match fsReader.GetItem path with
        | Ok (Some item) when item.Type = File ->
            openPath fsReader path.Parent (SelectItem (item, true)) (model.ClearStatus())
        | Ok _ ->
            openPath fsReader path SelectNone (model.ClearStatus())
        | Error e -> Error <| ActionError ("open path", e)
    | None -> Error <| InvalidPath pathStr

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
        openPath fsReader item.Path SelectNone (model.ClearStatus())
    | File ->
        let openError e = e |> actionError (sprintf "open '%s'" item.Name)
        let shortcutFolder = result {
            let! targetPath =
                if item.Path.Extension |> String.equalsIgnoreCase "lnk" then
                    fsReader.GetShortcutTarget item.Path |> openError
                    |> Result.map Path.Parse
                else
                    Ok None
            match targetPath with
            | Some targetPath ->
                let! target =
                    fsReader.GetItem targetPath |> openError
                    |> Result.bind (Result.ofOption (ShortcutTargetMissing (targetPath.Format model.PathFormat)))
                return if target.Type = Folder then Some target.Path else None
            | None ->
                return None
        }
        shortcutFolder
        |> Result.bind (function
            | Some shortcutFolder ->
                openPath fsReader shortcutFolder SelectNone (model.ClearStatus())
            | None ->
                os.OpenFile item.Path |> openError
                |> Result.map (fun () ->
                    fnAfterOpen |> Option.iter (fun f -> f())
                    model.WithStatus (MainStatus.openFile item.Name)
                )
        )
    | Empty -> Ok model

let openParent fsReader (model: MainModel) =
    if model.SearchCurrent.IsSome then
        if model.SelectedItem.Type = Empty then
            Ok (model |> clearSearchProps |> listDirectory SelectNone)
        else
            openPath fsReader model.SelectedItem.Path.Parent (SelectItem (model.SelectedItem, false)) (model.ClearStatus())
    else
        let rec getParent n (path: Path) (currentName: string) =
            if n < 1 || path = Path.Root then (path, currentName)
            else getParent (n-1) path.Parent path.Name
        let path, selectName = getParent model.RepeatCount model.Location model.Location.Name
        openPath fsReader path (SelectName selectName) (model.ClearStatus())

let refresh fsReader (model: MainModel) =
    openPath fsReader model.Location (SelectItem (model.SelectedItem, false)) model

let rec private shiftStacks current n fromStack toStack =
    match fromStack with
    | newCurrent :: fromStack when n > 0 ->
        shiftStacks newCurrent (n-1) fromStack (current :: toStack)
    | _ ->
        (current, fromStack, toStack)

let back fsReader model = result {
    if model.BackStack = [] then
        return model
    else
        let (path, cursor), fromStack, toStack =
            shiftStacks (model.Location, model.Cursor) model.RepeatCount model.BackStack model.ForwardStack
        let! model = openPath fsReader path (SelectIndex cursor) (model.ClearStatus())
        return { model with BackStack = fromStack; ForwardStack = toStack }
}

let forward fsReader model = result {
    if model.ForwardStack = [] then
        return model
    else
        let (path, cursor), fromStack, toStack =
            shiftStacks (model.Location, model.Cursor) model.RepeatCount model.ForwardStack model.BackStack
        let! model = openPath fsReader path (SelectIndex cursor) (model.ClearStatus())
        return { model with BackStack = toStack; ForwardStack = fromStack }
}

let sortList field model =
    let desc =
        match model.Sort with
        | Some (f, desc) when f = field -> not desc
        | _ -> field = Modified
    let selectType = if model.Cursor = 0 then SelectNone else SelectItem (model.SelectedItem, false)
    { model with
        Sort = Some (field, desc)
        Items = model.Items |> SortField.SortByTypeThen (field, desc)
        History = model.History.WithPathSort model.Location { Sort = field; Descending = desc }
    }
    |> fun m -> m.WithStatus (MainStatus.sort field desc)
    |> select selectType

let suggestPaths (fsReader: IFileSystemReader) (model: MainModel) = asyncSeq {
    let getSuggestions search (paths: Path list) =
        paths
        |> filterByTerms true false search (fun p -> p.Name)
        |> List.map (fun p -> p.FormatFolder model.PathFormat)
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
            let suggestions = pathsRes |> Result.map (getSuggestions search)
            yield { model with PathSuggestions = suggestions; PathSuggestCache = Some (dir, pathsRes) }
        | None ->
            yield { model with PathSuggestions = Ok [] }
    | None when model.LocationInput |> String.isNotEmpty ->
        let suggestions = getSuggestions model.LocationInput model.History.Paths
        yield { model with PathSuggestions = Ok suggestions }
    | None ->
        yield { model with PathSuggestions = Ok [] }
}

let deletePathSuggestion (path: Path) (model: MainModel) =
    // if location input does not contain a slash, the suggestions are from history
    if not (model.LocationInput |> String.contains "/" || model.LocationInput |> String.contains @"\") then
        let pathStr = path.FormatFolder model.PathFormat
        { model with
            History = { model.History with Paths = model.History.Paths |> List.filter ((<>) path) }
            PathSuggestions = model.PathSuggestions |> Result.map (List.filter ((<>) pathStr))
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
