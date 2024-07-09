module Koffee.MainLogic

open FSharp.Control
open VinylUI
open Acadian.FSharp
open Koffee.Main
open Koffee.Main.Util
open UIHelpers

let initModel (fsReader: IFileSystemReader) (screenBounds: Rectangle) startOptions model =
    let config = model.Config
    let windowRect =
        let location =
            startOptions.StartLocation |> Option.defaultWith (fun () ->
                let isFirstInstance =
                    System.Diagnostics.Process.GetProcesses()
                    |> Seq.where (fun p -> String.equalsIgnoreCase p.ProcessName "koffee")
                    |> Seq.length
                    |> (=) 1
                let (left, top) = config.Window.Location
                if isFirstInstance then (left, top) else (left + 30, top + 30)
            )
        let size = startOptions.StartSize |? config.Window.Size
        Rect.ofPairs location size |> Rect.fit screenBounds
    let model =
        { model with
            WindowLocation = windowRect.Location
            WindowSize = windowRect.Size
            SaveWindowSettings = startOptions.StartLocation.IsNone && startOptions.StartSize.IsNone
        }
    let history = model.History.Paths |> List.filter (fun hp -> hp.IsDirectory) |> List.map (fun hp -> hp.PathValue)
    let prevPath = history |> List.truncate 1
    let configPaths =
        match config.StartPath with
        | RestorePrevious -> prevPath @ [config.DefaultPath]
        | DefaultPath -> [config.DefaultPath] @ prevPath
    let paths = (startOptions.StartPath |> Option.toList) @ (configPaths @ [Path.Root] |> List.map string)
    let rec openPath error (paths: string list) =
        let withError = Option.foldBack MainModel.withError error
        match paths with
        | [] -> model |> withError
        | start :: paths ->
            match Nav.openUserPath fsReader start model with
            | Ok model ->
                let back =
                    (
                        match history with
                        | head :: tail when head = model.Location -> tail
                        | history -> history
                    )
                    |> List.truncate model.Config.Limits.Back
                    |> List.map (fun p -> (p, 0))
                { model with BackStack = back }
                |> withError
            | Error e ->
                openPath (Some (error |? e)) paths
    openPath None paths

let inputCharTyped fs subDirResults progress cancelInput char model = asyncSeqResult {
    let withBookmark char model =
        model
        |> MainModel.mapConfig (Config.withBookmark char model.Location)
        |> MainModel.withMessage (MainStatus.SetBookmark (char, model.LocationFormatted))
    let withSavedSearch char search model =
        model
        |> MainModel.mapConfig (Config.withSavedSearch char search)
        |> MainModel.withMessage (MainStatus.SetSavedSearch (char, search))
    match model.InputMode with
    | Some (Input CreateFile)
    | Some (Input CreateFolder)
    | Some (Input (Rename _)) ->
        if Path.InvalidNameChars |> String.contains (string char) then
            cancelInput()
    | Some (Input (Find _)) ->
        match KeyBinding.getKeysString FindNext |> Seq.toList with
        | [nextKey] when char = nextKey ->
            cancelInput ()
            yield Search.findNext model
        | _ when Path.InvalidNameChars |> String.contains (string char) ->
            cancelInput ()
        | _ -> ()
    | Some (Prompt mode) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match mode with
        | GoToBookmark ->
            match model.Config.GetBookmark char with
            | Some path ->
                yield model
                yield! model |> MainModel.clearStatus |> Nav.openPath fs path CursorStay
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoBookmark char)
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
                    model
                    |> MainModel.mapConfig (Config.withoutBookmark char)
                    |> MainModel.withMessage (MainStatus.DeletedBookmark (char, (path.Format model.PathFormat)))
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoBookmark char)
        | GoToSavedSearch ->
            match model.Config.GetSavedSearch char with
            | Some search ->
                yield!
                    { model with
                        InputText = search.Terms
                        SearchInput = search
                        SearchHistoryIndex = Some 0
                        History = model.History |> History.withSearch model.Config.Limits.PathHistory search
                    }
                    |> Search.search fs subDirResults progress
                    |> AsyncSeq.map Ok
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoSavedSearch char)
        | SetSavedSearch ->
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
        | DeleteSavedSearch ->
            match model.Config.GetSavedSearch char with
            | Some search ->
                yield
                    model
                    |> MainModel.mapConfig (Config.withoutSavedSearch char)
                    |> MainModel.withMessage (MainStatus.DeletedSavedSearch (char, search))
            | None ->
                yield model |> MainModel.withMessage (MainStatus.NoSavedSearch char)
    | Some (Confirm confirmType) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match char with
        | 'y' ->
            match confirmType with
            | Overwrite (putType, srcDestPairs) ->
                let itemRefs = srcDestPairs |> List.map (fun (src, _) -> src.Ref)
                let! model = Action.putInLocation fs progress false true putType itemRefs model
                yield { model with Config = { model.Config with YankRegister = None } }
            | Delete ->
                yield! Action.delete fs progress model.ActionItems model
            | OverwriteBookmark (char, _) ->
                yield withBookmark char model
            | OverwriteSavedSearch (char, _) ->
                match model.SearchCurrent with
                | Some search -> yield withSavedSearch char search model
                | None -> ()
        | 'n' ->
            let model = model |> MainModel.withMessage (MainStatus.CancelledConfirm confirmType)
            match confirmType with
            | Overwrite _ when not model.Config.ShowHidden && model.ActionItems |> List.exists (fun i -> i.IsHidden) ->
                // if we were temporarily showing a hidden file, refresh
                yield! Nav.refresh fs model
            | _ ->
                yield model
        | _ -> ()
    | _ -> ()
}

let inputChanged fsReader subDirResults progress model = asyncSeq {
    match model.InputMode with
    | Some (Input (Find _)) ->
        yield Search.find model
    | Some (Input Search) ->
        yield! Search.search fsReader subDirResults progress model
    | _ -> ()
}

let inputHistory offset model =
    match model.InputMode with
    | Some (Input Search) ->
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
            ShowHistoryType = index |> Option.map (cnst SearchHistory)
        }
    | _ -> model

let inputDelete isShifted cancelInput model =
    match model.InputMode with
    | Some (Prompt GoToBookmark)
    | Some (Prompt SetBookmark) ->
        cancelInput ()
        { model with InputMode = Some (Prompt DeleteBookmark) }
    | Some (Prompt GoToSavedSearch)
    | Some (Prompt SetSavedSearch) ->
        cancelInput ()
        { model with InputMode = Some (Prompt DeleteSavedSearch) }
    | Some (Input Search) when isShifted && model.ShowHistoryType = Some SearchHistory ->
        match model.SearchHistoryIndex with
        | Some index ->
            model
            |> MainModel.mapHistory (History.withoutSearchIndex index)
            |> inputHistory 0
        | None -> model
    | _ -> model

let submitInput fs os model = asyncSeqResult {
    match model.InputMode with
    | Some (Input (Find multi)) ->
        let model =
            { model with
                InputText = ""
                InputMode = if not multi || model.CursorItem.Type = File then None else model.InputMode
            }
        yield model
        yield! Nav.openItems fs os [model.CursorItem] model
    | Some (Input Search) ->
        let search = model.InputText |> Option.ofString |> Option.map (fun i -> { model.SearchInput with Terms = i })
        yield
            { model with
                InputMode = None
                SearchCurrent = if search.IsNone then None else model.SearchCurrent
                SearchHistoryIndex = Some 0
                History = model.History |> Option.foldBack (History.withSearch model.Config.Limits.SearchHistory) search
                ShowHistoryType = None
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
        yield! Action.rename fs model.CursorItem model.InputText model
    | _ -> ()
}

let cancelInput model =
    { model with InputMode = None; InputError = None }
    |> match model.InputMode with
        | Some (Input Search) -> Search.clearSearch
        | _ -> id

let escape model =
    if model.InputMode.IsSome then
        { model with InputMode = None }
    else if not model.KeyCombo.IsEmpty || model.RepeatCommand.IsSome then
        model |> MainModel.withoutKeyCombo
    else if model.ShowHistoryType.IsSome then
        { model with ShowHistoryType = None }
    else if not model.SelectedItems.IsEmpty then
        model |> MainModel.clearSelection
    else
        model.CancelToken.Cancel()
        model |> MainModel.clearStatus |> Search.clearSearch

let keyPress dispatcher (keyBindings: (KeyCombo * MainEvents) list) chord handleKey model = asyncSeq {
    let evt, modelFunc =
        match chord with
        | (ModifierKeys.None, Key.Escape) ->
            handleKey ()
            (None, escape)
        | (ModifierKeys.None, DigitKey digit) when model.KeyCombo = [] ->
            (None, MainModel.appendRepeatDigit digit)
        | _ ->
            let keyCombo = List.append model.KeyCombo [chord]
            match KeyBinding.getMatch keyBindings keyCombo with
            | KeyBinding.Match newEvent ->
                handleKey ()
                let modelFunc (model: MainModel) =
                    model
                    |> MainModel.withoutKeyCombo
                    // hide history if input prompt is opened
                    |> applyIf model.InputMode.IsSome (fun m -> { m with ShowHistoryType = None })
                (Some newEvent, modelFunc)
            | KeyBinding.PartialMatch ->
                handleKey ()
                (None, (fun m -> { m with KeyCombo = keyCombo }))
            | KeyBinding.NoMatch ->
                (None, MainModel.withoutKeyCombo)
    match evt with
    | Some e ->
        match dispatcher e with
        | Sync handler ->
            yield handler model |> modelFunc
        | Async handler ->
            yield! handler model |> AsyncSeq.map modelFunc
    | None ->
        yield modelFunc model
}

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
    if model.Config.Window.RefreshOnActivate then
        if model.IsSearchingSubFolders then
            yield model |> Nav.refreshDirectory fsReader
        else
            yield! model |> Search.refreshOrResearch fsReader subDirResults progress
}

let SyncResult handler =
    Sync (fun (model: MainModel) ->
        match handler model with
        | Ok newModel -> newModel
        | Error e -> model |> MainModel.withError e
    )

let AsyncResult handler =
    Async (fun (model: MainModel) -> asyncSeq {
        let mutable last = model
        for r in handler model |> AsyncSeq.takeWhileInclusive Result.isOk do
            match r with
            | Ok newModel ->
                last <- newModel
                yield newModel
            | Error e ->
                yield last |> MainModel.withError e
    })

type Controller(fs: IFileSystem, os, getScreenBounds, config: ConfigFile, history: HistoryFile, keyBindings,
                gridScroller, openSettings, closeWindow, startOptions) =
    let subDirResults = Event<_>()
    let progressEvt = Event<_>()
    let progress = Progress progressEvt

    let rec dispatcher evt =
        let handler =
            match evt with
            | KeyPress (chord, handler) -> Async (keyPress dispatcher keyBindings chord handler.Handle)
            | CursorUp -> Sync (fun m -> m |> MainModel.withCursorRel (-1 * m.RepeatCount))
            | CursorUpHalfPage -> Sync (fun m -> m |> MainModel.withCursorRel (-m.PageSize/2 * m.RepeatCount))
            | CursorDown -> Sync (fun m -> m |> MainModel.withCursorRel (1 * m.RepeatCount))
            | CursorDownHalfPage -> Sync (fun m -> m |> MainModel.withCursorRel (m.PageSize/2 * m.RepeatCount))
            | CursorToFirst -> Sync (fun m -> m |> MainModel.withCursor 0)
            | CursorToLast -> Sync (fun m -> m |> MainModel.withCursor (m.Items.Length - 1))
            | SelectToggle -> Sync Action.selectToggle
            | SelectRange -> Sync Action.selectRange
            | SelectAll -> Sync (fun m -> { m with SelectedItems = m.Items })
            | Scroll scrollType -> Sync (Nav.scrollView gridScroller scrollType)
            | OpenPath (path, handler) -> SyncResult (Nav.openInputPath fs os path handler)
            | OpenCursorItem -> AsyncResult (fun m -> Nav.openItems fs os [m.CursorItem] m)
            | OpenSelected -> AsyncResult (fun m -> Nav.openItems fs os m.ActionItems m)
            | OpenFileWith -> SyncResult (Command.openFileWith os)
            | OpenFileAndExit -> AsyncResult (Nav.openFilesAndExit fs os closeWindow)
            | OpenProperties -> SyncResult (Command.openProperties os)
            | OpenParent -> SyncResult (Nav.openParent fs)
            | OpenRoot -> SyncResult (Nav.openPath fs Path.Root CursorStay)
            | OpenDefault -> SyncResult (fun m -> Nav.openPath fs m.Config.DefaultPath CursorStay m)
            | Back -> Sync (Nav.back fs)
            | Forward -> Sync (Nav.forward fs)
            | Refresh -> AsyncResult (Search.refreshOrResearch fs subDirResults progress)
            | DeletePathSuggestion path -> Sync (Nav.deletePathSuggestion path)
            | Undo -> AsyncResult (Action.undo fs progress)
            | Redo -> AsyncResult (Action.redo fs progress)
            | ShowHistory typ -> Sync (fun m -> { m with ShowHistoryType = if m.ShowHistoryType <> Some typ then Some typ else None })
            | StartPrompt promptType -> SyncResult (Action.startInput fs (Prompt promptType))
            | StartConfirm confirmType -> SyncResult (Action.startInput fs (Confirm confirmType))
            | StartInput inputType -> SyncResult (Action.startInput fs (Input inputType))
            | InputCharTyped (c, handler) -> AsyncResult (inputCharTyped fs subDirResults progress handler.Handle c)
            | InputChanged -> Async (inputChanged fs subDirResults progress)
            | InputBack -> Sync (inputHistory 1)
            | InputForward -> Sync (inputHistory -1)
            | InputDelete (isShifted, handler) -> Sync (inputDelete isShifted handler.Handle)
            | SubDirectoryResults items -> Sync (Search.addSubDirResults items)
            | SubmitInput -> AsyncResult (submitInput fs os)
            | CancelInput -> Sync cancelInput
            | FindNext -> Sync Search.findNext
            | RepeatPreviousSearch -> Async (Search.repeatSearch fs subDirResults progress)
            | StartPut putType -> SyncResult (Action.registerSelectedItems putType)
            | ClearYank -> Sync (fun m -> { m with Config = { m.Config with YankRegister = None } })
            | Put -> AsyncResult (Action.put fs progress false)
            | ClipCopy -> SyncResult (Action.clipCopy os)
            | Recycle -> AsyncResult (fun m -> Action.recycle fs progress m.ActionItems m)
            | SortList field -> Sync (Nav.sortList field)
            | UpdateDropInPutType (paths, event) -> Sync (Command.updateDropInPutType paths event)
            | DropIn (paths, event) -> AsyncResult (Command.dropIn fs progress paths event)
            | DropOut putType -> Sync (Command.dropOut fs putType)
            | ToggleHidden -> Sync Command.toggleHidden
            | OpenSplitScreenWindow -> SyncResult (Command.openSplitScreenWindow os getScreenBounds)
            | OpenWithTextEditor -> SyncResult (Command.openWithTextEditor os)
            | OpenExplorer -> Sync (Command.openExplorer os)
            | OpenCommandLine -> SyncResult (Command.openCommandLine os)
            | OpenSettings -> SyncResult (Command.openSettings fs openSettings)
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
        match evt, handler with
        | KeyPress _, _
        | ConfigFileChanged _, _
        | HistoryFileChanged _, _ ->
            handler
        | _, Sync handler ->
            Sync (fun model ->
                if not model.IsStatusBusy
                then handler model
                else model
            )
        | _, Async handler ->
            Async (fun model -> asyncSeq {
                if not model.IsStatusBusy then
                    yield! handler model
            })

    member this.Start view =
        let model =
            { MainModel.Default with Config = config.Value; History = history.Value }
            |> initModel fs (getScreenBounds ()) startOptions
        let binder = MainView.binder config history progressEvt.Publish
        let events = MainView.events config history subDirResults.Publish
        Framework.start binder events dispatcher view model
