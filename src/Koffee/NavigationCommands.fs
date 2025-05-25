module Koffee.NavigationCommands

open System.Text.RegularExpressions
open FSharp.Control
open VinylUI
open Acadian.FSharp
open Koffee

let private setCursorAndSelected refreshSelected cursorMoveType (model: MainModel) =
    let cursor, pathsToSelect =
        match cursorMoveType with
        | CursorStay ->
            (model.Cursor, None)
        | CursorToIndex index ->
            (index, None)
        | CursorToPath (path, _) ->
            let cursor = model.Items |> List.tryFindIndex (fun item -> item.Path = path) |? model.Cursor
            (cursor, None)
        | CursorToAndSelectPaths (paths, _) ->
            let paths = Set paths
            let matches =
                model.Items
                |> Seq.map (fun item -> item.Path)
                |> Seq.indexed
                |> Seq.filter (fun (_, path) -> paths |> Set.contains path)
                |> Seq.toList
            match matches with
            | [] ->
                (model.Cursor, Some [])
            | (i, _) :: rest ->
                (i, if rest.IsEmpty then Some [] else Some (matches |> List.map snd))
    let pathsToSelect =
        pathsToSelect |> Option.orElseWith (fun () ->
            if refreshSelected
            then Some (model.SelectedItems |> List.map (fun item -> item.Path))
            else None
        )
    model
    |> MainModel.withCursor cursor
    |> Option.foldBack MainModel.selectItems pathsToSelect

let moveCursor = setCursorAndSelected false

let listDirectory cursor model =
    let possiblyHiddenPathsToSelect =
        match cursor with
        | CursorToPath (path, true) -> Set [path]
        | CursorToAndSelectPaths (paths, true) -> Set paths
        | _ -> Set.empty
    let items =
        model.Directory
        |> List.filter (fun i -> model.Config.ShowHidden || not i.IsHidden || possiblyHiddenPathsToSelect |> Set.contains i.Path)
        |> model.SortItems
        |> model.ItemsOrEmpty
    { model with Items = items } |> setCursorAndSelected true cursor

let private getDirectory (fsReader: IFileSystemReader) (model: MainModel) path =
    if path = Path.Network then
        model.History.NetHosts
        |> List.map (sprintf @"\\%s")
        |> List.choose Path.Parse
        |> List.map (fun path -> Item.Basic path path.Name NetHost)
        |> Ok
    else
        fsReader.GetItems path
        |> Result.mapError (fun e -> MainStatus.CouldNotOpenPath (path, e))

let openPath (fsReader: IFileSystemReader) path cursor (model: MainModel) =
    match getDirectory fsReader model path with
    | Ok directory ->
        { model with
            Directory = directory
            History = model.History |> History.withFolderPath model.Config.Limits.PathHistory path
            Sort = Some (model.History.FindSortOrDefault path |> PathSort.toTuple)
        }
        |> MainModel.withPushedLocation path
        |> clearSearchProps
        |> applyIf (path <> model.Location) MainModel.clearSelection
        |> listDirectory cursor
        |> Ok
    | Error e when path = model.Location ->
        { model with
            Directory = []
            Items = Item.EmptyFolderWithMessage (e.Message model.PathFormat) path
            SelectedItems = []
        }
        |> MainModel.withError e
        |> Ok
    | Error e ->
        Error e

let openUserPath (fsReader: IFileSystemReader) pathStr (model: MainModel) =
    match Path.Parse pathStr with
    | Some path ->
        match fsReader.GetItem path with
        | Ok (Some item) when item.Type = File ->
            model |> MainModel.clearStatus |> openPath fsReader path.Parent (CursorToPath (item.Path, true))
        | Ok _ ->
            model |> MainModel.clearStatus |> openPath fsReader path CursorStay
        | Error e ->
            Error <| MainStatus.ActionError ("open path", e)
    | None ->
        Error <| MainStatus.InvalidPath pathStr

let openInputPath fsReader os pathStr (keyHandler: KeyPressHandler) model = result {
    let pathStr = OsUtility.subEnvVars os pathStr
    let! model = openUserPath fsReader pathStr model
    keyHandler.Handle ()
    return model
}

let private getOpenTarget (fsReader: IFileSystemReader) pathFormat (item: Item) =
    match item.Type with
    | File ->
        let shortcutFolder = result {
            let! targetPath =
                if item.Path.Extension |> String.equalsIgnoreCase "lnk" then
                    fsReader.GetShortcutTarget item.Path
                    |> Result.map Path.Parse
                else
                    Ok None
            match targetPath with
            | Some targetPath ->
                let! targetOpt = fsReader.GetItem targetPath
                let! target =
                    targetOpt |> Result.ofOption (
                        ShortcutTargetMissingException(item.Name, targetPath.Format pathFormat) :> exn
                    )
                return if target.Type = Folder then Some (Folder, target.Path) else None
            | None ->
                return None
        }
        shortcutFolder |> Result.map (Option.defaultValue (item.Type, item.Path))
    | _ ->
        Ok (item.Type, item.Path)

let openItems fsReader (os: IOperatingSystem) items (model: MainModel) = asyncSeqResult {
    let getOpenTarget item =
        getOpenTarget fsReader model.PathFormat item |> Result.mapError (fun ex -> item.Name, ex)
    let openFile path =
        os.OpenFile path
        |> Result.map (fun () -> path)
        |> Result.mapError (fun ex -> path.Name, ex)
    match items with
    | [item] ->
        let mapError res = res |> Result.mapError (fun (name, ex) -> MainStatus.CouldNotOpenFiles [name, ex])
        let! itemType, path = getOpenTarget item |> mapError
        match itemType with
        | Folder | Drive | NetHost | NetShare ->
            yield! model |> MainModel.clearStatus |> openPath fsReader path CursorStay
        | File ->
            do! openFile item.Path |> mapError |> Result.map ignore
            yield
                model
                |> MainModel.mapHistory (History.withFilePaths model.Config.Limits.PathHistory [item.Path])
                |> MainModel.withMessage (MainStatus.OpenFiles [item.Name])
        | Empty -> ()
    | items ->
        let targets, targetErrors =
            items
            |> List.map getOpenTarget
            |> Result.partition
        let opened, openErrors =
            targets
            |> Seq.filter (fst >> (=) File)
            |> Seq.map (snd >> openFile)
            |> Result.partition
        let model = model |> MainModel.mapHistory (History.withFilePaths model.Config.Limits.PathHistory opened)
        match targetErrors @ openErrors with
        | [] ->
            if opened.IsEmpty then
                return MainStatus.NoFilesSelected
            else
                yield model |> MainModel.withMessage (MainStatus.OpenFiles (opened |> List.map (fun p -> p.Name)))
        | errors ->
            yield model
            return MainStatus.CouldNotOpenFiles errors
}

let openFilesAndExit fsReader os exit (model: MainModel) = asyncSeqResult {
    match model.ActionItems |> List.filter (fun i -> i.Type = File) with
    | [] ->
        return MainStatus.NoFilesSelected
    | files ->
        yield! openItems fsReader os files model
        exit ()
}

let openFileWith (os: IOperatingSystem) (model: MainModel) = result {
    match model.ActionItems with
    | [item] when item.Type = File ->
        do! os.OpenFileWith item.Path |> actionError "open file with"
        return model |> MainModel.withMessage (MainStatus.OpenFiles [item.Name])
    | [_] ->
        return model
    | _ ->
        return! Error MainStatus.CannotOpenWithMultiple
}

let openProperties (os: IOperatingSystem) (model: MainModel) = result {
    let items =
        model.ActionItems |> List.filter (fun i ->
            i.Type |> Seq.containedIn [File; Folder; Drive; NetShare]
            && not (i.Path = Path.Network)
        )
    if items.IsEmpty then
        return model
    else
        do! os.OpenProperties (items |> Seq.map (fun i -> i.Path)) |> actionError "open properties"
        return model |> MainModel.withMessage (MainStatus.OpenProperties (items |> List.map (fun i -> i.Name)))
}

let openWithTextEditor (os: IOperatingSystem) (model: MainModel) = result {
    let items = model.ActionItems |> List.filter (fun i -> i.Type = File)
    if items.IsEmpty then
        return model
    else
        let pathArg (path: Path) = path.Format Windows |> sprintf "\"%s\""
        let paths = items |> List.map (fun i -> i.Path)
        let args = paths |> Seq.map pathArg |> String.concat " "
        do! os.LaunchApp model.Config.TextEditor model.Location args
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Text editor", e))
        return
            model
            |> MainModel.mapHistory (History.withFilePaths model.Config.Limits.PathHistory paths)
            |> MainModel.withMessage (MainStatus.OpenTextEditor (items |> List.map (fun i -> i.Name)))
}

let openTerminal (os: IOperatingSystem) model = result {
    if model.Location <> Path.Root then
        do! os.LaunchApp model.Config.TerminalPath model.Location ""
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Terminal", e))
        return model |> MainModel.withMessage (MainStatus.OpenTerminal model.Location)
    else return model
}

let openExplorer (os: IOperatingSystem) (model: MainModel) = result {
    let parent = model.ActionItems.Head.Path.Parent
    let selectPaths =
        model.ActionItems
        |> Seq.filter (fun i -> i.Path.Parent = parent)
        |> Seq.map (fun i -> i.Path)
    do! os.OpenExplorer parent selectPaths |> actionError "open Explorer"
    return model |> MainModel.withMessage MainStatus.OpenExplorer
}

let openParent fsReader (model: MainModel) =
    if model.SearchCurrent.IsSome then
        if model.CursorItem.Type = Empty then
            Ok (model |> clearSearchProps |> listDirectory CursorStay)
        else
            model
            |> MainModel.clearStatus
            |> openPath fsReader model.CursorItem.Path.Parent model.KeepCursorByPath
    else
        let rec getParent n (path: Path) =
            if n < 1 || path.Parent = Path.Root then path
            else getParent (n-1) path.Parent
        let path = getParent (model.RepeatCount-1) model.Location
        model |> MainModel.clearStatus |> openPath fsReader path.Parent (CursorToPath (path, false))

let refresh fsReader (model: MainModel) =
    openPath fsReader model.Location model.KeepCursorByPath model

let refreshDirectory fsReader (model: MainModel) =
    getDirectory fsReader model model.Location
    |> function
        | Ok items -> items
        | Error e -> Item.EmptyFolderWithMessage (e.Message model.PathFormat) model.Location
    |> fun items -> { model with Directory = items }

let private ignoreError f model =
    f model |> Result.defaultValue model

let openPathIgnoreError fsReader path cursor = openPath fsReader path cursor |> ignoreError
let refreshIgnoreError fsReader = refresh fsReader |> ignoreError

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
        |> openPath fsReader path (CursorToIndex cursor)
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
        |> openPath fsReader path (CursorToIndex cursor)
        |> function
            | Ok model -> { model with BackStack = toStack; ForwardStack = fromStack }
            | Error e ->
                { model with ForwardStack = model.ForwardStack |> removeIndexOrLast (model.RepeatCount-1) }
                |> MainModel.withError e

let private getFilter showHidden searchInput =
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

let performSearch fsReader (subDirResults: Event<_>) progress (model: MainModel) = asyncSeq {
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
                    |> model.SortItems
                Cursor = 0
            } |> moveCursor model.KeepCursorByPath
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
        yield model |> listDirectory model.KeepCursorByPath
}

let addSubDirResults newItems model =
    let filter =
        model.SearchCurrent
        |> Option.filter (fun s -> s.SubFolders)
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
                |> performSearch fsReader subDirResults progress
        | None ->
            yield model |> MainModel.withError MainStatus.NoPreviousSearch
}

let traverseSearchHistory direction model =
    let offset =
        match direction with
        | Some InputBack -> 1
        | Some InputForward -> -1
        | None -> 0
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
        InputTextSelectionStartAndLength = SetValue (search.Terms.Length, 0)
        SearchInput = search
        SearchHistoryIndex = index
        HistoryDisplay = index |> Option.map (cnst SearchHistory)
    }

let deleteSearchHistory model =
    match model.SearchHistoryIndex with
    | Some index ->
        model
        |> MainModel.mapHistory (History.withoutSearchIndex index)
        |> traverseSearchHistory None
    | None -> model

let inputSearch (model: MainModel) =
    let inputText = model.SearchCurrent |> Option.map (fun s -> s.Terms) |? ""
    { model with
        InputMode = Some (Input Search)
        InputText = inputText
        InputTextSelectionStartAndLength = SetValue (0, inputText.Length)
    }

let withBookmark char model =
    model
    |> MainModel.mapConfig (Config.withBookmark char model.Location)
    |> MainModel.withMessage (MainStatus.SetBookmark (char, model.Location))

let withSavedSearch char search model =
    model
    |> MainModel.mapConfig (Config.withSavedSearch char search)
    |> MainModel.withMessage (MainStatus.SetSavedSearch (char, search))

let clearSearch (model: MainModel) =
    { model with HistoryDisplay = None }
    |> clearSearchProps
    |> listDirectory model.KeepCursorByPath

let refreshOrResearch fsReader subDirResults progress model = asyncSeqResult {
    match model.SearchCurrent with
    | Some current ->
        let cursor = model.KeepCursorByPath
        let! newModel = model |> openPath fsReader model.Location cursor
        let searchModels =
            { newModel with
                InputText = current.Terms
                SearchInput = current
                SearchHistoryIndex = model.SearchHistoryIndex
            }
            |> performSearch fsReader subDirResults progress
            |> AsyncSeq.cache
        yield! searchModels |> AsyncSeq.map Ok
        // when done searching, re-sort
        match! searchModels |> AsyncSeq.tryLast with
        | Some newModel ->
            let items = newModel.Items |> newModel.SortItems
            yield { newModel with Items = items } |> moveCursor cursor
        | None -> ()
    | None ->
        yield! refresh fsReader model
}

let sortList field model =
    let desc =
        match model.Sort with
        | Some (f, desc) when f = field -> not desc
        | _ -> field = Modified
    let cursor = if model.Cursor = 0 then CursorStay else model.KeepCursorByPath
    { model with
        Sort = Some (field, desc)
        Items = model.Items |> SortField.sortByTypeThen field desc
        History = model.History |> History.withPathSort model.Location { Sort = field; Descending = desc }
    }
    |> MainModel.withMessage (MainStatus.Sort (field, desc))
    |> moveCursor cursor

let private promptMark markType markCommand (model: MainModel) =
    { model with
        InputMode = Some (MarkPrompt (markType, markCommand))
        InputText = ""
    }

let private promptSetMark (model: MainModel) =
    let markType =
        if model.SearchCurrent.IsSome
        then SavedSearch
        else Bookmark
    promptMark markType SetMark model

let toggleHidden (model: MainModel) =
    let show = not model.Config.ShowHidden
    let model =
        { model with
            Config = { model.Config with ShowHidden = show }
        } |> MainModel.withMessage (MainStatus.ToggleHidden show)
    match model.SearchCurrent |> Option.bind (getFilter model.Config.ShowHidden >> Result.toOption) with
    | Some filter ->
        let items =
            model.Directory @ (model.SubDirectories |? [])
            |> filter
            |> model.SortItems
            |> model.ItemsOrEmpty
        { model with Items = items } |> moveCursor model.KeepCursorByPath
    | None ->
        model |> listDirectory model.KeepCursorByPath

let suggestPaths (fsReader: IFileSystemReader) (model: MainModel) = asyncSeq {
    let (|DirectorySearch|_|) input =
        input
        |> String.replace @"\" "/"
        |> String.lastIndexOf "/"
        |> Option.ofCond (flip (>=) 0)
        |> Option.bind (fun i -> option {
            let! dir = input |> String.substring 0 i |> Path.Parse
            let terms = input |> String.substringFrom (i + 1) |> parseSearchTerms
            return (dir, terms)
        })
    let (|Terms|_|) input =
        if input |> Seq.exists (Seq.containedIn ['\\'; '/'])
        then None
        else parseSearchTerms input
    let getSuggestions terms (paths: HistoryPath list) =
        paths
        |> filterByTerms true false terms (fun p -> p.PathValue.Name)
        |> Seq.truncate 20
        |> Seq.toList
    match model.LocationInput with
    | DirectorySearch (dir, terms) ->
        let! pathsRes =
            match model.PathSuggestCache with
            | Some (cachePath, cache) when cachePath = dir ->
                async { return cache }
            | _ ->
                runAsync (fun () ->
                    fsReader.GetFolders dir
                    |> Result.map (List.map (fun i -> i.HistoryPath))
                    |> Result.mapError (fun e -> e.Message)
                )
        let suggestions = pathsRes |> Result.map (Option.foldBack getSuggestions terms)
        yield { model with PathSuggestions = suggestions; PathSuggestCache = Some (dir, pathsRes) }
    | Terms terms ->
        let suggestions = getSuggestions terms model.History.Paths
        yield { model with PathSuggestions = Ok suggestions }
    | _ ->
        yield { model with PathSuggestions = Ok [] }
}

let deletePathSuggestion (path: HistoryPath) (model: MainModel) =
    // if location input does not contain a slash, the suggestions are from history
    if not (model.LocationInput |> String.contains "/" || model.LocationInput |> String.contains @"\") then
        { model with
            History = { model.History with Paths = model.History.Paths |> List.filter ((<>) path) }
            PathSuggestions = model.PathSuggestions |> Result.map (List.filter ((<>) path))
        }
    else
        model

let windowActivated fsReader subDirResults progress model = asyncSeqResult {
    if model.Config.Window.RefreshOnActivate then
        if model.IsSearchingSubFolders then
            yield model |> refreshDirectory fsReader
        else
            yield! model |> refreshOrResearch fsReader subDirResults progress
}

type Handler(
    fs: IFileSystemReader,
    os: IOperatingSystem,
    progress: Progress,
    subDirResults: Event<Item list>,
    closeWindow: unit -> unit
) =
    member _.LocationInputChanged model = suggestPaths fs model
    member _.LocationInputSubmit path handler model = openInputPath fs os path handler model
    member _.LocationInputCancel (model: MainModel) = { model with LocationInput = model.LocationFormatted }
    member _.DeletePathSuggestion path model = deletePathSuggestion path model
    member _.AddSubDirResults items model = addSubDirResults items model
    member _.WindowActivated model = windowActivated fs subDirResults progress model

    member _.Handle (command: NavigationCommand) =
        match command with
        | OpenCursorItem -> AsyncResult (fun m -> openItems fs os [m.CursorItem] m)
        | OpenSelected -> AsyncResult (fun m -> openItems fs os m.ActionItems m)
        | OpenFileWith -> SyncResult (openFileWith os)
        | OpenFileAndExit -> AsyncResult (openFilesAndExit fs os closeWindow)
        | OpenProperties -> SyncResult (openProperties os)
        | OpenWithTextEditor -> SyncResult (openWithTextEditor os)
        | OpenTerminal -> SyncResult (openTerminal os)
        | OpenExplorer -> SyncResult (openExplorer os)
        | OpenParent -> SyncResult (openParent fs)
        | OpenRoot -> SyncResult (openPath fs Path.Root CursorStay)
        | OpenDefault -> SyncResult (fun m -> openPath fs m.Config.DefaultPath CursorStay m)
        | Back -> Sync (back fs)
        | Forward -> Sync (forward fs)
        | Refresh -> AsyncResult (refreshOrResearch fs subDirResults progress)
        | StartSearch -> Sync inputSearch
        | RepeatPreviousSearch -> Async (repeatSearch fs subDirResults progress)
        | PromptGoToMark markType -> Sync (promptMark markType GoToMark)
        | PromptSetMark -> Sync promptSetMark
        | SortList field -> Sync (sortList field)
        | ToggleHidden -> Sync toggleHidden
        | ShowNavHistory -> Sync (MainModel.toggleHistoryDisplay NavHistory)
        | ShowUndoHistory -> Sync (MainModel.toggleHistoryDisplay UndoHistory)
        | ShowStatusHistory -> Sync (MainModel.toggleHistoryDisplay StatusHistory)

    member _.HandleFindInputEvent multi (evt: InputEvent) (model: MainModel) = asyncSeq {
        match evt with
        | InputChanged ->
            yield CursorCommands.find model
        | InputCharTyped (char, keyHandler) ->
            suppressInvalidPathChar char keyHandler
        | InputKeyPress (keyChord, keyHandler) ->
            match KeyBinding.getChordMatch model.Config.KeyBindings keyChord with
            | Some (Cursor FindNext) ->
                keyHandler.Handle()
                yield CursorCommands.findNext model
            | _ -> ()
        | InputSubmit ->
            let model =
                { model with
                    InputText = ""
                    InputMode = if not multi || model.CursorItem.Type = File then None else model.InputMode
                }
            yield model
            yield! model |> handleAsyncResult (openItems fs os [model.CursorItem])
        | _ -> ()
    }

    member _.HandleSearchInputEvent (evt: InputEvent) (model: MainModel) = asyncSeq {
        match evt with
        | InputChanged ->
            yield! performSearch fs subDirResults progress model
        | InputSubmit ->
            let search = model.InputText |> Option.ofString |> Option.map (fun i -> { model.SearchInput with Terms = i })
            yield
                { model with
                    InputMode = None
                    SearchCurrent = if search.IsNone then None else model.SearchCurrent
                    SearchHistoryIndex = Some 0
                    History = model.History |> Option.foldBack (History.withSearch model.Config.Limits.SearchHistory) search
                    HistoryDisplay = None
                }
        | InputNavigateHistory direction ->
            yield traverseSearchHistory (Some direction) model
        | InputDelete (isShifted, keyHandler) ->
            if isShifted && model.HistoryDisplay = Some SearchHistory then
                keyHandler.Handle()
                yield deleteSearchHistory model
        | _ -> ()
    }

    member _.HandleBookmarkCommand markCommand char (model: MainModel) = asyncSeqResult {
        match markCommand with
        | GoToMark ->
            match model.Config.GetBookmark char with
            | Some path ->
                yield model
                yield! model |> MainModel.clearStatus |> openPath fs path CursorStay
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoBookmark char)
        | SetMark ->
            match model.Config.GetBookmark char with
            | Some existingPath ->
                yield
                    { model with
                        InputMode = Some (Confirm (OverwriteBookmark (char, existingPath)))
                        InputText = ""
                    }
            | None ->
                yield withBookmark char model
        | DeleteMark ->
            match model.Config.GetBookmark char with
            | Some path ->
                yield
                    model
                    |> MainModel.mapConfig (Config.withoutBookmark char)
                    |> MainModel.withMessage (MainStatus.DeletedBookmark (char, path))
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoBookmark char)
    }

    member _.HandleSavedSearchCommand markCommand char (model: MainModel) = asyncSeqResult {
        match markCommand with
        | GoToMark ->
            match model.Config.GetSavedSearch char with
            | Some search ->
                yield!
                    { model with
                        InputText = search.Terms
                        SearchInput = search
                        SearchHistoryIndex = Some 0
                        History = model.History |> History.withSearch model.Config.Limits.PathHistory search
                    }
                    |> performSearch fs subDirResults progress
                    |> AsyncSeq.map Ok
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoSavedSearch char)
        | SetMark ->
            match model.SearchCurrent, model.Config.GetSavedSearch char with
            | Some _, Some existingSearch ->
                yield
                    { model with
                        InputMode = Some (Confirm (OverwriteSavedSearch (char, existingSearch)))
                        InputText = ""
                    }
            | Some search, None ->
                yield withSavedSearch char search model
            | None, _ -> ()
        | DeleteMark ->
            match model.Config.GetSavedSearch char with
            | Some search ->
                yield
                    model
                    |> MainModel.mapConfig (Config.withoutSavedSearch char)
                    |> MainModel.withMessage (MainStatus.DeletedSavedSearch (char, search))
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoSavedSearch char)
    }

    member _.ConfirmOverwriteBookmark char isYes model = asyncSeqResult {
        if isYes then
            withBookmark char model
    }

    member _.ConfirmOverwriteSavedSearch char isYes model = asyncSeqResult {
        if isYes then
            match model.SearchCurrent with
            | Some search -> yield withSavedSearch char search model
            | None -> ()
    }
