module Koffee.MainLogic

open FSharp.Control
open VinylUI
open Acadian.FSharp
open Koffee.Main
open Koffee.Main.Util

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
    let prevPath = model.History.Paths |> List.tryHead |> Option.toList
    let configPaths =
        match config.StartPath with
        | RestorePrevious -> prevPath @ [config.DefaultPath]
        | DefaultPath -> [config.DefaultPath] @ prevPath
    let paths = (startOptions.StartPath |> Option.toList) @ (configPaths @ [Path.Root] |> List.map string)
    let rec openPath error (paths: string list) =
        let withError (m: MainModel) =
            match error with
            | Some e -> m.WithError e
            | None -> m
        match paths with
        | [] -> model |> withError
        | start :: paths ->
            match Nav.openUserPath fsReader start model with
            | Ok model ->
                let back =
                    (
                        match model.History.Paths with
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
        { model with
            Config = model.Config.WithBookmark char model.Location
        } |> fun m -> m.WithMessage (MainStatus.SetBookmark (char, model.LocationFormatted))
    let withSavedSearch char search model =
        { model with
            Config = model.Config.WithSavedSearch char search
        } |> fun m -> m.WithMessage (MainStatus.SetSavedSearch (char, search))
    match model.InputMode with
    | Some (Input (CreateFile _))
    | Some (Input (CreateFolder _))
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
                yield! Nav.openPath fs path SelectNone (model.ClearStatus())
            | None ->
                yield model.WithMessage (MainStatus.NoBookmark char)
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
                    { model with Config = model.Config.WithoutBookmark char }
                    |> fun m -> m.WithMessage (MainStatus.DeletedBookmark (char, (path.Format model.PathFormat)))
            | None ->
                yield model.WithMessage (MainStatus.NoBookmark char)
        | GoToSavedSearch ->
            match model.Config.GetSavedSearch char with
            | Some search ->
                yield!
                    { model with
                        InputText = search.Terms
                        SearchInput = search
                        SearchHistoryIndex = Some 0
                        History = model.History.WithSearch model.Config.Limits.PathHistory search
                    }
                    |> Search.search fs subDirResults progress
                    |> AsyncSeq.map Ok
            | None ->
                yield model.WithMessage (MainStatus.NoSavedSearch char)
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
                    { model with Config = model.Config.WithoutSavedSearch char }
                    |> fun m -> m.WithMessage (MainStatus.DeletedSavedSearch (char, search))
            | None ->
                yield model.WithMessage (MainStatus.NoSavedSearch char)
    | Some (Confirm confirmType) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match char with
        | 'y' ->
            match confirmType with
            | Overwrite (putType, src, _) ->
                let! model = Action.putItem fs progress true src putType model
                yield { model with Config = { model.Config with YankRegister = None } }
            | Delete ->
                yield! Action.delete fs progress model.SelectedItem true model
            | OverwriteBookmark (char, _) ->
                yield withBookmark char model
            | OverwriteSavedSearch (char, _) ->
                match model.SearchCurrent with
                | Some search -> yield withSavedSearch char search model
                | None -> ()
        | 'n' ->
            let model = model.WithMessage MainStatus.Cancelled
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
            { model with History = model.History.WithoutSearchIndex index }
            |> inputHistory 0
        | None -> model
    | _ -> model

let submitInput fs os model = asyncSeqResult {
    match model.InputMode with
    | Some (Input (Find multi)) ->
        let model =
            { model with
                InputText = ""
                InputMode = if not multi || model.SelectedItem.Type = File then None else model.InputMode
            }
        yield model
        yield! Nav.openSelected fs os None model
    | Some (Input Search) ->
        let search = model.InputText |> Option.ofString |> Option.map (fun i -> { model.SearchInput with Terms = i })
        yield
            { model with
                InputMode = None
                SearchCurrent = if search.IsNone then None else model.SearchCurrent
                SearchHistoryIndex = Some 0
                History = search |> Option.map (model.History.WithSearch model.Config.Limits.SearchHistory) |? model.History
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
        yield! Action.rename fs model.SelectedItem model.InputText model
    | _ -> ()
}

let cancelInput model =
    { model with InputMode = None; InputError = None }
    |> match model.InputMode with
        | Some (Input Search) -> Search.clearSearch
        | _ -> id

let keyPress dispatcher (keyBindings: (KeyCombo * MainEvents) list) chord handleKey model = asyncSeq {
    let event, modelFunc =
        match chord with
        | (ModifierKeys.None, Key.Escape) ->
            handleKey ()
            let modelFunc m =
                if m.InputMode.IsSome then
                    { m with InputMode = None }
                else if not m.KeyCombo.IsEmpty || m.RepeatCommand.IsSome then
                    m.WithoutKeyCombo()
                else if m.ShowHistoryType.IsSome then
                    { m with ShowHistoryType = None }
                else
                    m.ClearStatus() |> Search.clearSearch
            (None, modelFunc)
        | (ModifierKeys.None, DigitKey digit) when model.KeyCombo = [] ->
            (None, (fun m -> m.AppendRepeatDigit digit))
        | _ ->
            let keyCombo = List.append model.KeyCombo [chord]
            match KeyBinding.getMatch keyBindings keyCombo with
            | KeyBinding.Match newEvent ->
                handleKey ()
                let modelFunc (m: MainModel) =
                    { m.WithoutKeyCombo() with
                        // hide history if input prompt is opened
                        ShowHistoryType = if m.InputMode.IsSome then None else m.ShowHistoryType
                    }
                (Some newEvent, modelFunc)
            | KeyBinding.PartialMatch ->
                handleKey ()
                (None, (fun m -> { m with KeyCombo = keyCombo }))
            | KeyBinding.NoMatch ->
                (None, (fun m -> m.WithoutKeyCombo()))
    match event with
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
    if model.Config.Window.RefreshOnActivate && not model.IsSearchingSubFolders then
        yield! model |> Search.refreshOrResearch fsReader subDirResults progress
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
                gridScroller, openSettings, closeWindow, startOptions) =
    let subDirResults = Event<_>()
    let progress = Event<_>()

    let rec dispatcher evt =
        let handler =
            match evt with
            | KeyPress (chord, handler) -> Async (keyPress dispatcher keyBindings chord handler.Handle)
            | CursorUp -> Sync (fun m -> m.WithCursorRel (-1 * m.RepeatCount))
            | CursorUpHalfPage -> Sync (fun m -> m.WithCursorRel (-m.PageSize/2 * m.RepeatCount))
            | CursorDown -> Sync (fun m -> m.WithCursorRel (1 * m.RepeatCount))
            | CursorDownHalfPage -> Sync (fun m -> m.WithCursorRel (m.PageSize/2 * m.RepeatCount))
            | CursorToFirst -> Sync (fun m -> m.WithCursor 0)
            | CursorToLast -> Sync (fun m -> m.WithCursor (m.Items.Length - 1))
            | Scroll scrollType -> Sync (Nav.scrollView gridScroller scrollType)
            | OpenPath (path, handler) -> SyncResult (Nav.openInputPath fs os path handler)
            | OpenSelected -> SyncResult (Nav.openSelected fs os None)
            | OpenFileWith -> SyncResult (Command.openFileWith os)
            | OpenFileAndExit -> SyncResult (Nav.openSelected fs os (Some closeWindow))
            | OpenProperties -> SyncResult (Command.openProperties os)
            | OpenParent -> SyncResult (Nav.openParent fs)
            | OpenRoot -> SyncResult (Nav.openPath fs Path.Root SelectNone)
            | OpenDefault -> SyncResult (fun m -> Nav.openPath fs m.Config.DefaultPath SelectNone m)
            | Back -> SyncResult (Nav.back fs)
            | Forward -> SyncResult (Nav.forward fs)
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
            | StartPut putType -> Sync (Action.registerItem putType)
            | ClearYank -> Sync (fun m -> { m with Config = { m.Config with YankRegister = None } })
            | Put -> AsyncResult (Action.put fs progress false)
            | ClipCopy -> SyncResult (Action.clipCopy os)
            | Recycle -> AsyncResult (Action.recycle fs progress)
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
        match handler, evt with
        | _, ConfigFileChanged _
        | _, HistoryFileChanged _ -> handler
        | Sync handler, _ ->
            Sync (fun model ->
                if not model.IsStatusBusy then
                    handler model
                else
                    model
            )
        | Async handler, _ ->
            Async (fun model -> asyncSeq {
                if not model.IsStatusBusy then
                    yield! handler model
            })

    member this.Start view =
        let model =
            { MainModel.Default with Config = config.Value; History = history.Value }
            |> initModel fs (getScreenBounds ()) startOptions
        let binder = MainView.binder config history progress.Publish
        let events = MainView.events config history subDirResults.Publish
        Framework.start binder events dispatcher view model
