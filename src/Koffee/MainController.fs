module Koffee.MainLogic

open System.Text.RegularExpressions
open System.Threading.Tasks
open FSharp.Control
open VinylUI
open Acadian.FSharp

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

let actionError actionName = Result.mapError (fun e -> ActionError (actionName, e))
let itemActionError item pathFormat = Result.mapError (fun e -> ItemActionError (item, pathFormat, e))

let private runAsync (f: unit -> 'a) = f |> Task.Run |> Async.AwaitTask

let private filterByTerms sort caseSensitive search proj items =
    let terms =
        search
        |> String.trim
        |> Option.ofString
        |> Option.map (String.split ' ' >> Array.toList)
    match terms with
    | Some terms ->
        let contains = if caseSensitive then String.contains else String.containsIgnoreCase
        let startsWith = if caseSensitive then String.startsWith else String.startsWithIgnoreCase
        let matches = items |> List.filter (fun i -> terms |> List.forall (fun t -> proj i |> contains t))
        if sort then
            matches |> List.sortBy (fun i ->
                let weight = if proj i |> startsWith terms.[0] then 0 else 1
                (weight, proj i |> String.toLower)
            )
        else
            matches
    | None -> items

let clearSearchProps model =
    model.SubDirectoryCancel.Cancel()
    { model with
        CurrentSearch = None
        SearchHistoryIndex = -1
        SubDirectories = None
        Progress = None
    }

let private itemsInDirOrNetHost (fsReader: IFileSystemReader) path history =
    if path = Path.Network then
        history.NetHosts
        |> List.map (sprintf @"\\%s")
        |> List.choose Path.Parse
        |> List.map (fun path -> Item.Basic path path.Name NetHost)
        |> Ok
    else
        fsReader.GetItems path |> actionError "open path"

module Nav =
    let select selectType (model: MainModel) =
        model.WithCursor (
            match selectType with
            | SelectIndex index -> index
            | SelectName name ->
                model.Items |> List.tryFindIndex (fun i -> String.equalsIgnoreCase i.Name name) |? model.Cursor
            | SelectNone -> model.Cursor
        )

    let listDirectory selectType model =
        let selectName =
            match selectType with
            | SelectName name -> name
            | _ -> ""
        let items =
            model.Directory
            |> List.filter (fun i -> model.Config.ShowHidden || not i.IsHidden ||
                                     String.equalsIgnoreCase selectName i.Name)
            |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
            |> Seq.ifEmpty (Item.EmptyFolder model.Location)
        { model with Items = items } |> select selectType

    let openPath (fsReader: IFileSystemReader) path select (model: MainModel) = result {
        let! directory = itemsInDirOrNetHost fsReader path model.History
        return
            { model.WithBackStackedLocation path with
                Directory = directory
                History = model.History.WithPathAndNetHost path
                Status = None
                Sort = Some (model.History.FindSortOrDefault path |> PathSort.toTuple)
                Cursor = 0
            } |> clearSearchProps
              |> listDirectory select
    }

    let openUserPath (fsReader: IFileSystemReader) pathStr model =
        match Path.Parse pathStr with
        | Some path ->
            match fsReader.GetItem path with
            | Ok (Some item) when item.Type = File ->
                openPath fsReader path.Parent (SelectName item.Name) model
            | Ok _ ->
                openPath fsReader path SelectNone model
            | Error e -> Error <| ActionError ("open path", e)
        | None -> Error <| InvalidPath pathStr

    let openInputPath fsReader os (evtHandler: EvtHandler) model = result {
        let pathStr = OsUtility.subEnvVars os model.LocationInput
        let! model = openUserPath fsReader pathStr model
        evtHandler.Handle ()
        return model
    }

    let openSelected fsReader (os: IOperatingSystem) fnAfterOpen (model: MainModel) =
        let item = model.SelectedItem
        match item.Type with
        | Folder | Drive | NetHost | NetShare ->
            openPath fsReader item.Path SelectNone model
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
                    openPath fsReader shortcutFolder SelectNone model
                | None ->
                    os.OpenFile item.Path |> openError
                    |> Result.map (fun () ->
                        fnAfterOpen |> Option.iter (fun f -> f())
                        { model with Status = Some <| MainStatus.openFile item.Name }
                    )
            )
        | Empty -> Ok model

    let openParent fsReader (model: MainModel) =
        if model.CurrentSearch.IsSome then
            if model.SelectedItem.Type = Empty then
                Ok (model |> clearSearchProps |> listDirectory SelectNone)
            else
                openPath fsReader model.SelectedItem.Path.Parent (SelectName model.SelectedItem.Name) model
        else
            openPath fsReader model.Location.Parent (SelectName model.Location.Name) model

    let refresh fsReader (model: MainModel) =
        openPath fsReader model.Location SelectNone model
        |> Result.map (fun newModel ->
            if model.Status.IsSome then
                { newModel with Status = model.Status }
            else newModel
        )

    let rec private shiftStacks current n fromStack toStack =
        match fromStack with
        | newCurrent :: fromStack when n > 0 ->
            shiftStacks newCurrent (n-1) fromStack (current :: toStack)
        | _ ->
            (current, fromStack, toStack)

    let private openShiftedHistoryPath fsReader model stacksGetter stacksSetter = result {
        let repeatCount = model.RepeatCount |? 1

        let (path, cursor), fromStack, toStack =
            shiftStacks (model.Location, model.Cursor) repeatCount <|| stacksGetter model

        let! model = openPath fsReader path (SelectIndex cursor) model

        return stacksSetter model fromStack toStack
    }

    let back fsReader model = result {
        if model.BackStack = [] then
            return model
        else
            return! openShiftedHistoryPath fsReader model
                (fun m -> m.BackStack, m.ForwardStack) 
                (fun m bs fs -> { m with BackStack = bs; ForwardStack = fs })
    }

    let forward fsReader model = result {
        if model.ForwardStack = [] then
            return model
        else
            return! openShiftedHistoryPath fsReader model
                (fun m -> m.ForwardStack, m.BackStack) 
                (fun m fs bs -> { m with ForwardStack = fs; BackStack = bs })
    }

    let sortList field model =
        let desc =
            match model.Sort with
            | Some (f, desc) when f = field -> not desc
            | _ -> field = Modified
        let selectType = if model.Cursor = 0 then SelectNone else SelectName model.SelectedItem.Name
        { model with
            Sort = Some (field, desc)
            Items = model.Items |> SortField.SortByTypeThen (field, desc)
            Status = Some <| MainStatus.sort field desc
            History = model.History.WithPathSort model.Location { Sort = field; Descending = desc }
        } |> select selectType

    let toggleHidden fsReader (model: MainModel) = asyncSeqResult {
        let model = { model with Config = { model.Config with ShowHidden = not model.Config.ShowHidden } }
        yield model
        let select =
            if not model.Config.ShowHidden && model.SelectedItem.IsHidden then
                SelectNone
            else
                SelectName model.SelectedItem.Name
        let! model = model |> openPath fsReader model.Location select
        yield { model with Status = Some <| MainStatus.toggleHidden model.Config.ShowHidden }
    }

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

module Search =
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
            |> List.skip ((model.RepeatCount |? 1) - 1)
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
            { model with Status = Some <| MainStatus.find prefix model.RepeatCount }
            |> moveCursorTo true false (fun i -> i.Name |> String.startsWithIgnoreCase prefix)
        | None -> model

    let private getFilter model =
        model.InputText
        |> Option.ofString
        |> Option.bind (fun input ->
            if model.SearchRegex then
                let options = if model.SearchCaseSensitive then RegexOptions.None else RegexOptions.IgnoreCase
                try
                    let re = Regex(input, options)
                    Some (List.filter (fun item -> re.IsMatch item.Name))
                with :? System.ArgumentException -> None
            else
                Some (filterByTerms false model.SearchCaseSensitive input (fun item -> item.Name))
        )

    let private enumerateSubDirs (fsReader: IFileSystemReader) (subDirResults: Event<_>) (progress: Event<_>)
                                 isCancelled items = async {
        let getDirs = List.filter (fun t -> t.Type = Folder)
        let rec enumerate progressFactor dirs = async {
            for dir in dirs do
                if not <| isCancelled () then
                    let! subItemsRes = runAsync (fun () -> fsReader.GetItems dir.Path)
                    if not <| isCancelled () then
                        let subItems = subItemsRes |> Result.toOption |? []
                        let subDirs = getDirs subItems
                        let progressFactor = progressFactor / float (subDirs.Length + 1)
                        subDirResults.Trigger subItems
                        progress.Trigger (Some progressFactor)
                        do! enumerate progressFactor subDirs
        }
        let dirs = getDirs items
        do! enumerate (1.0 / float dirs.Length) dirs
        progress.Trigger None
    }

    let search fsReader subDirResults progress (model: MainModel) = asyncSeq {
        let current = Some (model.InputText, model.SearchCaseSensitive, model.SearchRegex,
                            model.SearchSubFolders)
        let model = { model with CurrentSearch = current }
        match model.InputText |> String.isNotEmpty, getFilter model with
        | true, Some filter ->
            let withItems items model =
                let noResults = [ { Item.Empty with Name = "<No Search Results>"; Path = model.Location } ]
                { model with
                    Items = items |> Seq.ifEmpty noResults
                    Cursor = 0
                } |> Nav.select (SelectName model.SelectedItem.Name)
            let items = model.Directory |> filter
            if model.SearchSubFolders then
                match model.SubDirectories with
                | None ->
                    let cancelToken = CancelToken()
                    yield
                        { model with
                            SubDirectories = Some []
                            SubDirectoryCancel = cancelToken
                            Sort = None
                        } |> withItems items
                    do! enumerateSubDirs fsReader subDirResults progress cancelToken.get_IsCancelled model.Directory
                | Some subDirs ->
                    let items = items @ filter subDirs |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
                    yield model |> withItems items
            else
                model.SubDirectoryCancel.Cancel()
                yield { model with SubDirectories = None } |> withItems items
        | true, None ->
            yield model
        | false, _ ->
            yield model |> Nav.listDirectory (SelectName model.SelectedItem.Name)
    }

    let addSubDirResults newItems model =
        let filter = getFilter model |? cnst []
        let items =
            match filter newItems, model.Items with
            | [], _ -> model.Items
            | matches, [item] when item.Type = Empty -> matches
            | matches, _ -> model.Items @ matches
        { model with
            SubDirectories = model.SubDirectories |> Option.map (flip (@) newItems)
            Items = items
        }

    let clearSearch model =
        model
        |> clearSearchProps
        |> Nav.listDirectory (SelectName model.SelectedItem.Name)

module Action =
    let private performedAction action model =
        { model with
            UndoStack = action :: model.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete action model.PathFormat
        }

    let private setInputSelection cursorPos model =
        let fullLen = model.InputText.Length
        let nameLen =
            if model.SelectedItem.Type = File then
                Path.SplitName model.InputText |> fst |> String.length
            else
                fullLen
        { model with
            InputTextSelection =
                match cursorPos with
                | Begin -> (0, 0)
                | EndName -> (nameLen, 0)
                | End -> (fullLen, 0)
                | ReplaceName -> (0, nameLen)
                | ReplaceAll -> (0, fullLen)
        }

    let startInput (fsReader: IFileSystemReader) inputMode (model: MainModel) = result {
        let! allowed =
            match inputMode with
            | Input CreateFile
            | Input CreateFolder ->
                if model.IsSearchingSubFolders then
                    Error CannotPutHere
                else
                    match fsReader.GetItem model.Location with
                    | Ok (Some item) when item.Type.CanCreateIn -> Ok true
                    | Ok _ -> Error <| CannotPutHere
                    | Error e -> Error <| ActionError ("create item", e)
            | Input (Rename _)
            | Confirm Delete -> Ok model.SelectedItem.Type.CanModify
            | Prompt SetBookmark when model.IsSearchingSubFolders -> Ok false
            | _ -> Ok true
        if allowed then
            let model = { model with InputMode = Some inputMode }
            match inputMode with
            | Input Search ->
                let input, sub = model.CurrentSearch |> Option.map (fun (s, _, _, sub) -> (s, sub)) |? ("", false)
                return { model with InputText = input; SearchSubFolders = sub }
                       |> setInputSelection End
            | Input (Rename pos) ->
                return { model with InputText = model.SelectedItem.Name }
                       |> setInputSelection pos
            | _ ->
                return { model with InputText = "" }
        else
            return model
    }

    let create (fs: IFileSystem) itemType name model = asyncSeqResult {
        let createPath = model.Location.Join name
        let action = CreatedItem (Item.Basic createPath name itemType)
        let! existing = fs.GetItem createPath |> itemActionError action model.PathFormat
        match existing with
        | None ->
            do! fs.Create itemType createPath |> itemActionError action model.PathFormat
            let! model = Nav.openPath fs model.Location (SelectName name) model
            yield model |> performedAction (CreatedItem model.SelectedItem)
        | Some existing ->
            yield! Nav.openPath fs model.Location (SelectName existing.Name) model
            return CannotUseNameAlreadyExists ("create", itemType, name, existing.IsHidden)
    }

    let undoCreate (fs: IFileSystem) item model = asyncSeqResult {
        if fs.IsEmpty item.Path then
            yield { model with Status = Some <| MainStatus.undoingCreate item }
            let! res = runAsync (fun () -> fs.Delete item.Path)
            do! res |> itemActionError (DeletedItem (item, true)) model.PathFormat
            if model.Location = item.Path.Parent then
                yield! Nav.refresh fs model
        else
            return CannotUndoNonEmptyCreated item
    }

    let rename (fs: IFileSystem) item newName (model: MainModel) = result {
        if item.Type.CanModify then
            let action = RenamedItem (item, newName)
            let newPath = item.Path.Parent.Join newName
            let! existing =
                if String.equalsIgnoreCase item.Name newName then Ok None
                else fs.GetItem newPath |> itemActionError action model.PathFormat
            match existing with
            | None ->
                do! fs.Move item.Path newPath |> itemActionError action model.PathFormat
                let newItem = { item with Name = newName; Path = newPath }
                let substitute = List.map (fun i -> if i = item then newItem else i)
                return
                    { model with Directory = model.Directory |> substitute }
                    |> (
                        if model.CurrentSearch.IsSome then
                            fun m -> { m with Items = m.Items |> substitute }
                        else
                            Nav.listDirectory (SelectName newName)
                    )
                    |> performedAction action
            | Some existingItem ->
                return! Error <| CannotUseNameAlreadyExists ("rename", item.Type, newName, existingItem.IsHidden)
        else return model
    }

    let undoRename (fs: IFileSystem) oldItem currentName (model: MainModel) = result {
        let parentPath = oldItem.Path.Parent
        let currentPath = parentPath.Join currentName
        let item = { oldItem with Name = currentName; Path = currentPath }
        let action = RenamedItem (item, oldItem.Name)
        let! existing =
            if String.equalsIgnoreCase oldItem.Name currentName then Ok None
            else fs.GetItem oldItem.Path |> itemActionError action model.PathFormat
        match existing with
        | None ->
            do! fs.Move currentPath oldItem.Path |> itemActionError action model.PathFormat
            return! Nav.openPath fs parentPath (SelectName oldItem.Name) model
        | Some existingItem ->
            return! Error <| CannotUseNameAlreadyExists ("rename", oldItem.Type, oldItem.Name, existingItem.IsHidden)
    }

    let registerItem action (model: MainModel) =
        if model.SelectedItem.Type.CanModify then
            let reg = Some (model.SelectedItem.Path, model.SelectedItem.Type, action)
            { model with
                Config = { model.Config with YankRegister = reg }
                Status = None
            }
        else model

    let getCopyName name i =
        let (nameNoExt, ext) = Path.SplitName name
        let number = if i = 0 then "" else sprintf " %i" (i+1)
        sprintf "%s (copy%s)%s" nameNoExt number ext

    let putItem (fs: IFileSystem) overwrite item putAction model = asyncSeqResult {
        let sameFolder = item.Path.Parent = model.Location
        match! fs.GetItem model.Location |> actionError "put item" with
        | Some container when container.Type.CanCreateIn ->
            if putAction = Move && sameFolder then
                return CannotMoveToSameFolder
        | _ -> return CannotPutHere
        let! newName =
            match putAction with
            | Copy when sameFolder ->
                let unused name =
                    match fs.GetItem (model.Location.Join name) with
                    | Ok None -> true
                    | _ -> false
                Seq.init 99 (getCopyName item.Name)
                |> Seq.tryFind unused
                |> Result.ofOption (TooManyCopies item.Name)
            | Shortcut ->
                Ok (item.Name + ".lnk")
            | _ ->
                Ok item.Name
        let newPath = model.Location.Join newName
        let action = PutItem (putAction, item, newPath)
        let fileSysAction =
            match putAction with
            | Move -> fs.Move
            | Copy -> fs.Copy
            | Shortcut -> fs.CreateShortcut
        let! existing = fs.GetItem newPath |> itemActionError action model.PathFormat
        match existing with
        | Some existing when not overwrite ->
            // refresh item list to make sure we can see the existing file
            let! model = Nav.openPath fs model.Location (SelectName existing.Name) model
            yield
                { model with
                    InputMode = Some (Confirm (Overwrite (putAction, item, existing)))
                    InputText = ""
                }
        | _ ->
            yield { model with Status = MainStatus.runningAction action model.PathFormat }
            let! res = runAsync (fun () -> fileSysAction item.Path newPath)
            do! res |> itemActionError action model.PathFormat
            let! model = Nav.openPath fs model.Location (SelectName newName) model
            yield model |> performedAction action
    }

    let put (fs: IFileSystem) overwrite (model: MainModel) = asyncSeqResult {
        match model.Config.YankRegister with
        | None -> ()
        | Some (path, _, putAction) ->
            if model.IsSearchingSubFolders then
                return CannotPutHere
            match! fs.GetItem path |> actionError "read yank register item" with
            | Some item ->
                let! model = putItem fs overwrite item putAction model
                if model.InputMode.IsNone then
                    yield { model with Config = { model.Config with YankRegister = None } }
            | None ->
                return YankRegisterItemMissing (path.Format model.PathFormat)
    }

    let undoMove (fs: IFileSystem) item currentPath (model: MainModel) = asyncSeqResult {
        let from = { item with Path = currentPath; Name = currentPath.Name }
        let action = PutItem (Move, from, item.Path)
        let! existing = fs.GetItem item.Path |> itemActionError action model.PathFormat
        match existing with
        | Some _ ->
            // TODO: prompt for overwrite here?
            return CannotUndoMoveToExisting item
        | None ->
            yield { model with Status = Some <| MainStatus.undoingMove item }
            let! res = runAsync (fun () -> fs.Move currentPath item.Path)
            do! res |> itemActionError action model.PathFormat
            yield! Nav.openPath fs item.Path.Parent (SelectName item.Name) model
    }

    let undoCopy (fs: IFileSystem) item (currentPath: Path) (model: MainModel) = asyncSeqResult {
        let copyModified =
            match fs.GetItem currentPath with
            | Ok (Some copy) -> copy.Modified
            | _ -> None
        let isDeletionPermanent =
            match item.Modified, copyModified with
            | Some orig, Some copy when orig = copy -> true
            | _ -> false
        let action = DeletedItem ({ item with Path = currentPath }, isDeletionPermanent)
        let fileSysFunc = if isDeletionPermanent then fs.Delete else fs.Recycle
        yield { model with Status = Some <| MainStatus.undoingCopy item isDeletionPermanent }
        let! res = runAsync (fun () -> fileSysFunc currentPath)
        do! res |> itemActionError action model.PathFormat
        if model.Location = currentPath.Parent then
            yield! Nav.refresh fs model
    }

    let undoShortcut (fs: IFileSystem) shortcutPath (model: MainModel) = result {
        let action = DeletedItem ({ Item.Empty with Path = shortcutPath; Name = shortcutPath.Name; Type = File }, true)
        do! fs.Delete shortcutPath |> itemActionError action model.PathFormat
        if model.Location = shortcutPath.Parent then
            return! Nav.refresh fs model
        else
            return model
    }

    let clipCopy (os: IOperatingSystem) (model: MainModel) = result {
        let item = model.SelectedItem
        match item.Type with
        | File | Folder ->
            do! os.CopyToClipboard item.Path |> actionError "copy to clipboard"
        | _ -> ()
        return { model with Status = Some (MainStatus.clipboardCopy (item.Path.Format model.PathFormat)) }
    }

    let delete (fsWriter: IFileSystemWriter) item permanent (model: MainModel) = asyncSeqResult {
        if item.Type.CanModify then
            let action = DeletedItem (item, permanent)
            let fileSysFunc = if permanent then fsWriter.Delete else fsWriter.Recycle
            yield { model with Status = MainStatus.runningAction action model.PathFormat }
            let! res = runAsync (fun () -> fileSysFunc item.Path)
            do! res |> itemActionError action model.PathFormat
            yield
                { model with
                    Directory = model.Directory |> List.except [item]
                    Items = model.Items |> List.except [item] |> Seq.ifEmpty (Item.EmptyFolder model.Location)
                }.WithCursor model.Cursor
                |> performedAction action
    }

    let recycle fsWriter (model: MainModel) = asyncSeqResult {
        if model.SelectedItem.Type = NetHost then
            let host = model.SelectedItem.Name
            yield
                { model with
                    Directory = model.Directory |> List.except [model.SelectedItem]
                    Items = model.Items |> List.except [model.SelectedItem] |> Seq.ifEmpty (Item.EmptyFolder model.Location)
                    History = model.History.WithoutNetHost host
                    Status = Some <| MainStatus.removedNetworkHost host
                }.WithCursor model.Cursor
        else
            yield! delete fsWriter model.SelectedItem false model
    }

    let rec private undoStack fs model count iter = asyncSeqResult {
        match model.UndoStack with
        | action :: rest ->
            let model = { model with UndoStack = rest }
            yield model
            let! model =
                match action with
                | CreatedItem item ->
                    undoCreate fs item model
                | RenamedItem (oldItem, curName) ->
                    undoRename fs oldItem curName model
                    |> AsyncSeq.singleton
                | PutItem (Move, item, newPath) ->
                    undoMove fs item newPath model
                | PutItem (Copy, item, newPath) ->
                    undoCopy fs item newPath model
                | PutItem (Shortcut, item, newPath) ->
                    undoShortcut fs newPath model
                    |> AsyncSeq.singleton
                | DeletedItem (item, permanent) ->
                    Error (CannotUndoDelete (permanent, item))
                    |> AsyncSeq.singleton
            let model =
                { model with
                    RedoStack = action :: model.RedoStack
                    Status = Some <| MainStatus.undoAction action model.PathFormat (Some count) iter
                }
            yield model
            if iter < count then
                yield! undoStack fs model count (iter + 1)
        | [] -> return NoUndoActions
    }

    let undo fs model = undoStack fs model (model.RepeatCount |? 1) 1

    let rec private redoStack fs model count iter = asyncSeqResult {
        match model.RedoStack with
        | action :: rest ->
            let model = { model with RedoStack = rest }
            yield model
            let goToPath (itemPath: Path) =
                let path = itemPath.Parent
                if path <> model.Location then
                    Nav.openPath fs path SelectNone model
                else Ok model
            let! model = asyncSeqResult {
                match action with
                | CreatedItem item ->
                    let! model = goToPath item.Path
                    yield! create fs item.Type item.Name model
                | RenamedItem (item, newName) ->
                    let! model = goToPath item.Path
                    yield! rename fs item newName model
                | PutItem (putAction, item, newPath) ->
                    let! model = goToPath newPath
                    yield { model with Status = MainStatus.redoingAction action model.PathFormat }
                    yield! putItem fs false item putAction model
                | DeletedItem (item, permanent) ->
                    let! model = goToPath item.Path
                    yield { model with Status = MainStatus.redoingAction action model.PathFormat }
                    yield! delete fs item permanent model
            }
            let model =
                { model with
                    RedoStack = rest
                    Status = Some <| MainStatus.redoAction action model.PathFormat (Some count) iter
                }
            yield model
            if iter < count then
                yield! redoStack fs model count (iter + 1)
        | [] -> return NoRedoActions
    }

    let redo fs model = redoStack fs model (model.RepeatCount |? 1) 1

    let openSplitScreenWindow (os: IOperatingSystem) getScreenBounds model = result {
        let mapFst f t = (fst t |> f, snd t)
        let fitRect = Rect.ofPairs model.WindowLocation (model.WindowSize |> mapFst ((*) 2))
                      |> Rect.fit (getScreenBounds())
        let model =
            { model with
                WindowLocation = fitRect.Location
                WindowSize = fitRect.Size |> mapFst (flip (/) 2)
            }

        let left, top = model.WindowLocation
        let width, height = model.WindowSize
        let path = (model.Location.Format Windows).TrimEnd([|'\\'|])
        let args = sprintf "\"%s\" --location=%i,%i --size=%i,%i"
                           path (left + width) top width height

        let! koffeePath = Path.Parse (System.Reflection.Assembly.GetExecutingAssembly().Location)
                          |> Result.ofOption CouldNotFindKoffeeExe
        let folder = koffeePath.Parent
        do! os.LaunchApp (koffeePath.Format Windows) folder args
            |> Result.mapError (fun e -> CouldNotOpenApp ("Koffee", e))
        return model
    }

    let openExplorer (os: IOperatingSystem) (model: MainModel) =
        os.OpenExplorer model.SelectedItem
        { model with Status = Some <| MainStatus.openExplorer }

    let openFileWith (os: IOperatingSystem) (model: MainModel) = result {
        let item = model.SelectedItem
        match item.Type with
        | File ->
            do! os.OpenFileWith item.Path |> actionError "open file with"
            return { model with Status = Some <| MainStatus.openFile item.Name }
        | _ ->
            return model
    }

    let openProperties (os: IOperatingSystem) (model: MainModel) = result {
        let item = model.SelectedItem
        match item.Type with
        | File | Folder ->
            do! os.OpenProperties item.Path |> actionError "open properties"
            return { model with Status = Some <| MainStatus.openProperties item.Name }
        | _ ->
            return model
    }

    let openCommandLine (os: IOperatingSystem) model = result {
        if model.Location <> Path.Root then
            do! os.LaunchApp model.Config.CommandlinePath model.Location ""
                |> Result.mapError (fun e -> CouldNotOpenApp ("Commandline tool", e))
            return { model with Status = Some <| MainStatus.openCommandLine model.LocationFormatted }
        else return model
    }

    let openWithTextEditor (os: IOperatingSystem) (model: MainModel) = result {
        match model.SelectedItem.Type with
        | File ->
            let args = model.SelectedItem.Path.Format Windows |> sprintf "\"%s\""
            do! os.LaunchApp model.Config.TextEditor model.Location args
                |> Result.mapError (fun e -> CouldNotOpenApp ("Text Editor", e))
            return { model with Status = Some <| MainStatus.openTextEditor model.SelectedItem.Name }
        | _ -> return model
    }

    let openSettings fsReader openSettings model = result {
        let config = openSettings model.Config
        return! { model with Config = config } |> Nav.refresh fsReader
    }

let initModel (fsReader: IFileSystemReader) startOptions model =
    let config = model.Config
    let model =
        { model with
            WindowLocation =
                startOptions.StartLocation |> Option.defaultWith (fun () ->
                    let isFirstInstance =
                        System.Diagnostics.Process.GetProcesses()
                        |> Seq.where (fun p -> String.equalsIgnoreCase p.ProcessName "koffee")
                        |> Seq.length
                        |> (=) 1
                    let (left, top) = config.Window.Location
                    if isFirstInstance then (left, top) else (left + 30, top + 30)
                )
            WindowSize = startOptions.StartSize |? config.Window.Size
            SaveWindowSettings = startOptions.StartLocation.IsNone && startOptions.StartSize.IsNone
        }
    let prevPath = model.History.Paths |> List.tryHead |> Option.toList
    let configPaths =
        match config.StartPath with
        | RestorePrevious -> prevPath @ [config.DefaultPath]
        | DefaultPath -> [config.DefaultPath] @ prevPath
    let paths = (startOptions.StartPath |> Option.toList) @ (configPaths |> List.map string)
    let rec openPath error (paths: string list) =
        let withError (m: MainModel) =
            match error with
            | Some e -> m.WithError e
            | None -> m
        match paths with
        | [] -> model |> withError
        | start :: paths ->
            let prevPath = paths |> Seq.choose Path.Parse |> Seq.tryHead |? Path.Root
            match Nav.openUserPath fsReader start { model with Location = prevPath } with
            | Ok model -> model |> withError
            | Error e -> openPath (Some (error |? e)) paths
    openPath None paths

let refreshOrResearch fsReader subDirResults progress model = asyncSeqResult {
    match model.CurrentSearch with
    | Some currentSearch ->
        let! newModel = model |> Nav.openPath fsReader model.Location (SelectName model.SelectedItem.Name)
        let input, case, regex, sub = currentSearch
        yield!
            { newModel with
                InputText = input
                SearchCaseSensitive = case
                SearchRegex = regex
                SearchSubFolders = sub
                SearchHistoryIndex = model.SearchHistoryIndex
            }
            |> Search.search fsReader subDirResults progress
            |> AsyncSeq.map Ok
    | None ->
        yield! Nav.refresh fsReader model
}

let inputCharTyped fs cancelInput char model = asyncSeqResult {
    let withBookmark char model =
        { model with
            Config = model.Config.WithBookmark char model.Location
            Status = Some <| MainStatus.setBookmark char model.LocationFormatted
        }
    match model.InputMode with
    | Some (Input (Find _)) ->
        if char = ';' then // TODO: read key binding?
            cancelInput ()
            yield Search.findNext model
    | Some (Prompt mode) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match mode with
        | GoToBookmark ->
            match model.Config.GetBookmark char with
            | Some path ->
                yield model
                yield! Nav.openPath fs path SelectNone model
            | None ->
                yield { model with Status = Some <| MainStatus.noBookmark char }
        | SetBookmark ->
            match model.Config.GetBookmark char with
            | Some existingPath ->
                yield
                    { model with
                        InputMode = Some (Confirm (OverwriteBookmark (char, existingPath)))
                        InputText = ""
                    }
            | None ->
                yield withBookmark char model
        | DeleteBookmark ->
            match model.Config.GetBookmark char with
            | Some path ->
                yield
                    { model with
                        Config = model.Config.WithoutBookmark char
                        Status = Some <| MainStatus.deletedBookmark char (path.Format model.PathFormat)
                    }
            | None ->
                yield { model with Status = Some <| MainStatus.noBookmark char }
    | Some (Confirm confirmType) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match char with
        | 'y' ->
            match confirmType with
            | Overwrite (action, src, _) ->
                let! model = Action.putItem fs true src action model
                yield { model with Config = { model.Config with YankRegister = None } }
            | Delete ->
                yield! Action.delete fs model.SelectedItem true model
            | OverwriteBookmark (char, _) ->
                yield withBookmark char model
        | 'n' ->
            let model = { model with Status = Some <| MainStatus.cancelled }
            match confirmType with
            | Overwrite _ when not model.Config.ShowHidden && model.SelectedItem.IsHidden ->
                // if we were temporarily showing a hidden file, refresh
                yield! Nav.refresh fs model
            | _ ->
                yield model
        | _ -> ()
    | _ -> ()
}

let inputChanged fsReader subDirResults progress model = asyncSeq {
    match model.InputMode with
    | Some (Input (Find _)) when String.isNotEmpty model.InputText ->
        yield Search.find model.InputText model
    | Some (Input Search) ->
        yield! Search.search fsReader subDirResults progress model
    | _ -> ()
}

let inputHistory offset model =
    match model.InputMode with
    | Some (Input Search) ->
        let index = model.SearchHistoryIndex + offset |> max -1 |> min (model.History.Searches.Length-1)
        let input, case, regex =
            if index < 0 then
                ("", model.SearchCaseSensitive, model.SearchRegex)
            else
                model.History.Searches.[index]
        { model with
            InputText = input
            InputTextSelection = (input.Length, 0)
            SearchCaseSensitive = case
            SearchRegex = regex
            SearchHistoryIndex = index
        }
    | _ -> model

let inputDelete cancelInput model =
    match model.InputMode with
    | Some (Prompt GoToBookmark) | Some (Prompt SetBookmark) ->
        cancelInput ()
        { model with InputMode = Some (Prompt DeleteBookmark) }
    | _ -> model

let submitInput fs os model = asyncSeqResult {
    match model.InputMode with
    | Some (Input (Find multi)) ->
        let model =
            if multi then { model with InputText = ""  }
            else { model with InputMode = None }
        yield model
        yield! Nav.openSelected fs os None model
    | Some (Input Search) ->
        let search =
            model.InputText
            |> Option.ofString
            |> Option.map (fun i -> (i, model.SearchCaseSensitive, model.SearchRegex))
        yield
            { model with
                InputMode = None
                CurrentSearch = if search.IsNone then None else model.CurrentSearch
                SearchHistoryIndex = 0
                History = search |> Option.map model.History.WithSearch |? model.History
            }
    | Some (Input CreateFile) ->
        let model = { model with InputMode = None }
        yield model
        yield! Action.create fs File model.InputText model
    | Some (Input CreateFolder) ->
        let model = { model with InputMode = None }
        yield model
        yield! Action.create fs Folder model.InputText model
    | Some (Input (Rename _)) ->
        let model = { model with InputMode = None }
        yield model
        yield! Action.rename fs model.SelectedItem model.InputText model
    | _ -> ()
}

let cancelInput model =
    { model with InputMode = None }
    |>  match model.InputMode with
        | Some (Input Search) -> Search.clearSearch
        | _ -> id

let keyPress dispatcher (keyBindings: (KeyCombo * MainEvents) list) chord handleKey model = asyncSeq {
    let event, modelAlteration =
        match chord with
        | (ModifierKeys.None, Key.Escape) ->
            handleKey ()
            let modelAlteration m =
                if m.InputMode.IsSome then
                    { m with InputMode = None }
                else if not m.KeyCombo.IsEmpty || m.RepeatCount.IsSome then
                    m.WithoutKeyCombo()
                else
                    { m with Status = None } |> Search.clearSearch
            (None, modelAlteration)
        | (ModifierKeys.None, DigitKey digit) when model.KeyCombo = [] ->
            (None, (fun m -> m.AppendRepeatDigit digit))
        | _ ->
            let keyCombo = List.append model.KeyCombo [chord]
            match KeyBinding.getMatch keyBindings keyCombo with
            | KeyBinding.Match newEvent ->
                handleKey ()
                (Some newEvent, (fun m -> m.WithoutKeyCombo()))
            | KeyBinding.PartialMatch ->
                handleKey ()
                (None, (fun m -> { m with KeyCombo = keyCombo }))
            | KeyBinding.NoMatch ->
                (None, (fun m -> m.WithoutKeyCombo()))
    match event with
    | Some e ->
        match dispatcher e with
        | Sync handler ->
            let newModel = handler model
            yield modelAlteration newModel
        | Async handler ->
            yield! handler model |> AsyncSeq.map modelAlteration
    | None ->
        yield modelAlteration model
}

let addProgress incr model =
    { model with Progress = incr |> Option.map ((+) (model.Progress |? 0.0)) }

let windowLocationChanged location model =
    let config =
        if model.SaveWindowSettings then
            let window = { model.Config.Window with Location = location }
            { model.Config with Window = window }
        else model.Config
    { model with WindowLocation = location; Config = config }

let windowSizeChanged size model =
    let config =
        if model.SaveWindowSettings then
            let window = { model.Config.Window with Size = size }
            { model.Config with Window = window }
        else model.Config
    { model with WindowSize = size; Config = config }

let windowMaximized maximized model =
    let config =
        if model.SaveWindowSettings then
            let window = { model.Config.Window with IsMaximized = maximized }
            { model.Config with Window = window }
        else model.Config
    { model with Config = config }

let windowActivated fsReader subDirResults progress model = asyncSeqResult {
    if model.Config.Window.RefreshOnActivate && not model.IsSearchingSubFolders then
        yield! model |> refreshOrResearch fsReader subDirResults progress
    else
        yield model
}

let SyncResult handler =
    Sync (fun (model: MainModel) ->
        match handler model with
        | Ok m -> m
        | Error e -> model.WithError e
    )

let AsyncResult handler =
    Async (fun (model: MainModel) -> asyncSeq {
        let mutable last = model
        for r in handler model |> AsyncSeq.takeWhileInclusive Result.isOk do
            match r with
            | Ok m ->
                last <- m
                yield m
            | Error e ->
                yield last.WithError e
    })

type Controller(fs: IFileSystem, os, getScreenBounds, config: ConfigFile, history: HistoryFile, keyBindings,
                openSettings, closeWindow, startOptions) =
    let subDirResults = Event<_>()
    let progress = Event<_>()

    let mulRepeatCount m value =
        match m.RepeatCount with
        | Some count -> value * count
        | None -> value

    let repeatByRepeatCount f m =
        AsyncSeq.repeatResult (m.RepeatCount |? 1) f m

    let rec dispatcher evt =
        let handler =
            match evt with
            | KeyPress (chord, handler) -> Async (keyPress dispatcher keyBindings chord handler.Handle)
            | CursorUp -> Sync (fun m -> m.WithCursorRel (mulRepeatCount m -1))
            | CursorUpHalfPage -> Sync (fun m -> m.WithCursorRel (mulRepeatCount m -m.HalfPageSize))
            | CursorDown -> Sync (fun m -> m.WithCursorRel (mulRepeatCount m 1))
            | CursorDownHalfPage -> Sync (fun m -> m.WithCursorRel (mulRepeatCount m m.HalfPageSize))
            | CursorToFirst -> Sync (fun m -> m.WithCursor 0)
            | CursorToLast -> Sync (fun m -> m.WithCursor (m.Items.Length - 1))
            | OpenPath handler -> SyncResult (Nav.openInputPath fs os handler)
            | OpenSelected -> SyncResult (Nav.openSelected fs os None)
            | OpenFileWith -> SyncResult (Action.openFileWith os)
            | OpenFileAndExit -> SyncResult (Nav.openSelected fs os (Some closeWindow))
            | OpenProperties -> SyncResult (Action.openProperties os)
            | OpenParent -> SyncResult (Nav.openParent fs)
            | Back -> SyncResult (Nav.back fs)
            | Forward -> SyncResult (Nav.forward fs)
            | Refresh -> AsyncResult (refreshOrResearch fs subDirResults progress)
            | Undo -> AsyncResult (Action.undo fs)
            | Redo -> AsyncResult (Action.redo fs)
            | StartPrompt promptType -> SyncResult (Action.startInput fs (Prompt promptType))
            | StartConfirm confirmType -> SyncResult (Action.startInput fs (Confirm confirmType))
            | StartInput inputType -> SyncResult (Action.startInput fs (Input inputType))
            | InputCharTyped (c, handler) -> AsyncResult (inputCharTyped fs handler.Handle c)
            | InputChanged -> Async (inputChanged fs subDirResults progress)
            | InputBack -> Sync (inputHistory 1)
            | InputForward -> Sync (inputHistory -1)
            | InputDelete handler -> Sync (inputDelete handler.Handle)
            | SubDirectoryResults items -> Sync (Search.addSubDirResults items)
            | SubmitInput -> AsyncResult (submitInput fs os)
            | CancelInput -> Sync cancelInput
            | AddProgress incr -> Sync (addProgress incr)
            | FindNext -> Sync Search.findNext
            | StartAction action -> Sync (Action.registerItem action)
            | ClearYank -> Sync (fun m -> { m with Config = { m.Config with YankRegister = None } })
            | Put -> AsyncResult (Action.put fs false)
            | ClipCopy -> SyncResult (Action.clipCopy os)
            | Recycle -> AsyncResult (Action.recycle fs)
            | SortList field -> Sync (Nav.sortList field)
            | ToggleHidden -> AsyncResult (Nav.toggleHidden fs)
            | OpenSplitScreenWindow -> SyncResult (Action.openSplitScreenWindow os getScreenBounds)
            | OpenWithTextEditor -> SyncResult (Action.openWithTextEditor os)
            | OpenExplorer -> Sync (Action.openExplorer os)
            | OpenCommandLine -> SyncResult (Action.openCommandLine os)
            | OpenSettings -> SyncResult (Action.openSettings fs openSettings)
            | Exit -> Sync (fun m -> closeWindow(); m)
            | LocationInputChanged -> Async (Nav.suggestPaths fs)
            | ResetLocationInput -> Sync (fun m -> { m with LocationInput = m.LocationFormatted })
            | ConfigFileChanged config -> Sync (fun m -> { m with Config = config })
            | HistoryFileChanged history -> Sync (fun m -> { m with History = history })
            | PageSizeChanged size -> Sync (fun m -> { m with PageSize = size })
            | WindowLocationChanged (l, t) -> Sync (windowLocationChanged (l, t))
            | WindowSizeChanged (w, h) -> Sync (windowSizeChanged (w, h))
            | WindowMaximizedChanged maximized -> Sync (windowMaximized maximized)
            | WindowActivated -> AsyncResult (windowActivated fs subDirResults progress)
        let isBusy model =
            match model.Status with
            | Some (Busy _) -> true
            | _ -> false
        match handler, evt with
        | _, ConfigFileChanged _
        | _, HistoryFileChanged _ -> handler
        | Sync handler, _ ->
            Sync (fun model ->
                if not (isBusy model) then
                    handler model
                else
                    model
            )
        | Async handler, _ ->
            Async (fun model -> asyncSeq {
                if not (isBusy model) then
                    yield! handler model
            })

    member this.Start view =
        let model =
            { MainModel.Default with Config = config.Value; History = history.Value }
            |> initModel fs startOptions
        let events = MainView.events config history subDirResults.Publish progress.Publish
        Framework.start (MainView.binder config history) events dispatcher view model
