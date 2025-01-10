module Koffee.MainController

open FSharp.Control
open VinylUI
open Acadian.FSharp
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
            match NavigationCommands.openUserPath fsReader start model with
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
        |> MainModel.withMessage (MainStatus.SetBookmark (char, model.Location))
    let withSavedSearch char search model =
        model
        |> MainModel.mapConfig (Config.withSavedSearch char search)
        |> MainModel.withMessage (MainStatus.SetSavedSearch (char, search))
    match model.InputMode with
    | Some (Input NewFile)
    | Some (Input NewFolder)
    | Some (Input (Rename _)) ->
        if Path.InvalidNameChars |> String.contains (string char) then
            cancelInput()
    | Some (Input (Find _)) ->
        if KeyBinding.getKeysString (Cursor FindNext) |> Seq.toList = [char] then
            cancelInput ()
            yield CursorCommands.findNext model
        else if Path.InvalidNameChars |> String.contains (string char) then
            cancelInput()
    | Some (Prompt mode) ->
        cancelInput ()
        let model = { model with InputMode = None }
        match mode with
        | GoToBookmark ->
            match model.Config.GetBookmark char with
            | Some path ->
                yield model
                yield! model |> MainModel.clearStatus |> NavigationCommands.openPath fs path CursorStay
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
                    |> MainModel.withMessage (MainStatus.DeletedBookmark (char, path))
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
                    |> NavigationCommands.search fs subDirResults progress
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
                let! model = ItemActionCommands.Put.putInLocation fs progress false true putType itemRefs model
                yield { model with Config = { model.Config with YankRegister = None } }
            | Delete ->
                yield! ItemActionCommands.Delete.delete fs progress model.ActionItems model
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
                yield! NavigationCommands.refresh fs model
            | _ ->
                yield model
        | _ -> ()
    | _ -> ()
}

let inputChanged fsReader subDirResults progress model = asyncSeq {
    match model.InputMode with
    | Some (Input (Find _)) ->
        yield CursorCommands.find model
    | Some (Input Search) ->
        yield! NavigationCommands.search fsReader subDirResults progress model
    | _ -> ()
}

let inputHistory offset model =
    match model.InputMode with
    | Some (Input Search) -> NavigationCommands.traverseSearchHistory offset model
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
    | Some (Input Search) when isShifted && model.HistoryDisplay = Some SearchHistory ->
        NavigationCommands.deleteSearchHistory model
    | _ ->
        model

let submitInput fs os model = asyncSeqResult {
    match model.InputMode with
    | Some (Input inputType) ->
        match inputType with
        | Find multi ->
            let model =
                { model with
                    InputText = ""
                    InputMode = if not multi || model.CursorItem.Type = File then None else model.InputMode
                }
            yield model
            yield! NavigationCommands.openItems fs os [model.CursorItem] model
        | Search ->
            let search = model.InputText |> Option.ofString |> Option.map (fun i -> { model.SearchInput with Terms = i })
            yield
                { model with
                    InputMode = None
                    SearchCurrent = if search.IsNone then None else model.SearchCurrent
                    SearchHistoryIndex = Some 0
                    History = model.History |> Option.foldBack (History.withSearch model.Config.Limits.SearchHistory) search
                    HistoryDisplay = None
                }
        | NewFile | NewFolder ->
            let model = { model with InputMode = None }
            yield model
            let itemType = if inputType = NewFolder then Folder else File
            yield! ItemActionCommands.Create.create fs itemType model.InputText model
        | Rename _ ->
            let model = { model with InputMode = None }
            yield model
            yield! ItemActionCommands.Rename.rename fs model.CursorItem model.InputText model
    | _ -> ()
}

let cancelInput model =
    { model with InputMode = None; InputError = None }
    |> applyIf (model.InputMode = Some (Input Search)) NavigationCommands.clearSearch

let private escape model =
    if model.InputMode.IsSome then
        cancelInput model
    else if not model.KeyCombo.IsEmpty || model.RepeatCommand.IsSome then
        model |> MainModel.withoutKeyCombo
    else if model.HistoryDisplay.IsSome then
        { model with HistoryDisplay = None }
    else if not model.SelectedItems.IsEmpty then
        model |> MainModel.clearSelection
    else
        model.CancelToken.Cancel()
        model |> MainModel.clearStatus |> NavigationCommands.clearSearch

let keyPress handleCommand (keyBindings: (KeyCombo * MainCommand) list) chord handleKey model = asyncSeq {
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
                (Some newEvent, MainModel.withoutKeyCombo)
            | KeyBinding.PartialMatch ->
                handleKey ()
                (None, (fun m -> { m with KeyCombo = keyCombo }))
            | KeyBinding.NoMatch ->
                (None, MainModel.withoutKeyCombo)
    match evt with
    | Some e ->
        match handleCommand e with
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

type Controller(
    cursorHandler: CursorCommands.Handler,
    navigationHandler: NavigationCommands.Handler,
    itemActionHandler: ItemActionCommands.Handler,
    windowHandler: WindowCommands.Handler,
    fs: IFileSystem,
    os: IOperatingSystem,
    getScreenBounds: unit -> Rectangle,
    progress: Progress,
    subDirResults: Event<Item list>,
    keyBindings: (KeyCombo * MainCommand) list,
    configFile: ConfigFile,
    historyFile: HistoryFile,
    startOptions: StartOptions
) =
    let handleCommand (command: MainCommand) =
        match command with
        | Cursor command -> cursorHandler.Handle command
        | Navigation command -> navigationHandler.Handle command
        | ItemAction command -> itemActionHandler.Handle command
        | Window command -> windowHandler.Handle command

    let dispatcher evt =
        let handler =
            match evt with
            | KeyPress (chord, handler) -> Async (keyPress handleCommand keyBindings chord handler.Handle)
            | ItemDoubleClick -> handleCommand (Navigation OpenCursorItem)
            | SettingsButtonClick -> handleCommand (Window OpenSettings)
            | LocationInputChanged -> Async navigationHandler.SuggestPaths
            | LocationInputSubmit (path, handler) -> SyncResult (navigationHandler.OpenInputPath path handler)
            | LocationInputCancel -> Sync (fun m -> { m with LocationInput = m.LocationFormatted })
            | DeletePathSuggestion path -> Sync (navigationHandler.DeletePathSuggestion path)
            // TODO: delegate input events to command handlers / create interface for input modes to handle char typed, submit, cancel etc
            | InputCharTyped (c, handler) -> AsyncResult (inputCharTyped fs subDirResults progress handler.Handle c)
            | InputChanged -> Async (inputChanged fs subDirResults progress)
            | InputBack -> Sync (inputHistory 1)
            | InputForward -> Sync (inputHistory -1)
            | InputDelete (isShifted, handler) -> Sync (inputDelete isShifted handler.Handle)
            | InputSubmit -> AsyncResult (submitInput fs os)
            | InputCancel -> Sync cancelInput
            | SubDirectoryResults items -> Sync (navigationHandler.AddSubDirResults items)
            | UpdateDropInPutType (paths, event) -> Sync (itemActionHandler.UpdateDropInPutType paths event)
            | DropIn (paths, event) -> AsyncResult (itemActionHandler.DropIn paths event)
            | DropOut event -> Sync (itemActionHandler.DropOut event)
            | WindowActivated -> AsyncResult navigationHandler.WindowActivated
            | Background (ConfigFileChanged config) -> Sync (fun m -> { m with Config = config })
            | Background (HistoryFileChanged history) -> Sync (fun m -> { m with History = history })
            | Background (PageSizeChanged size) -> Sync (fun m -> { m with PageSize = size })
            | Background (WindowLocationChanged (l, t)) -> Sync (windowLocationChanged (l, t))
            | Background (WindowSizeChanged (w, h)) -> Sync (windowSizeChanged (w, h))
            | Background (WindowMaximizedChanged maximized) -> Sync (windowMaximized maximized)

        if evt.IsBackground then
            handler
        else
            match handler with
            | Sync handler ->
                Sync (fun model ->
                    if not model.IsStatusBusy
                    then handler model
                    else model
                )
            | Async handler ->
                Async (fun model -> asyncSeq {
                    if not model.IsStatusBusy then
                        yield! handler model
                })

    member this.Start window =
        let model =
            { MainModel.Default with Config = configFile.Value; History = historyFile.Value }
            |> initModel fs (getScreenBounds ()) startOptions
        let binder = MainView.binder configFile historyFile progress.Observable
        let events = MainView.events configFile historyFile subDirResults.Publish
        Framework.start binder events dispatcher window model
