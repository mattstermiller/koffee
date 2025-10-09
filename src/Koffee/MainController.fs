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

let keyPress handleCommand chord handleKey model = asyncSeq {
    let command, modelFunc =
        match chord with
        | (ModifierKeys.None, Key.Escape) ->
            handleKey ()
            (None, escape)
        | (ModifierKeys.None, KeyBindingLogic.DigitKey digit) when model.KeyCombo = [] ->
            (None, MainModel.appendRepeatDigit digit)
        | _ ->
            let keyCombo = List.append model.KeyCombo [chord]
            match KeyBindingLogic.getMatch model.Config.KeyBindings keyCombo with
            | KeyBindingLogic.Match newEvent ->
                handleKey ()
                (Some newEvent, MainModel.withoutKeyCombo)
            | KeyBindingLogic.PartialMatch ->
                handleKey ()
                (None, (fun m -> { m with KeyCombo = keyCombo }))
            | KeyBindingLogic.NoMatch ->
                (None, MainModel.withoutKeyCombo)
    match command with
    | Some cmd ->
        match handleCommand cmd with
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
    getScreenBounds: unit -> Rectangle,
    progress: Progress,
    subDirResults: Event<Item list>,
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

    let handleMarkPromptEvent markType markCommand evt model = asyncSeq {
        match evt with
        | InputCharTyped (char, keyHandler) ->
            keyHandler.Handle()
            let model = { model with InputMode = None }
            let handler =
                match markType with
                | Bookmark -> navigationHandler.HandleBookmarkCommand
                | SavedSearch -> navigationHandler.HandleSavedSearchCommand
            yield! model |> handleAsyncResult (handler markCommand char)
        | InputKeyPress ((_, Key.Delete), keyHandler) ->
            keyHandler.Handle()
            let prompt = MarkPrompt (markType, DeleteMark)
            yield { model with InputMode = Some prompt }
        | _ -> ()
    }

    let handleConfirmEvent confirmType evt model = asyncSeq {
        match evt with
        | InputCharTyped (char, keyHandler) ->
            keyHandler.Handle()
            let handler =
                match confirmType with
                | Overwrite (putType, srcExistingPairs) -> itemActionHandler.ConfirmOverwrite putType srcExistingPairs
                | Delete -> itemActionHandler.ConfirmDelete
                | OverwriteBookmark (char, _) -> navigationHandler.ConfirmOverwriteBookmark char
                | OverwriteSavedSearch (char, _) -> navigationHandler.ConfirmOverwriteSavedSearch char
            let model = { model with InputMode = None }
            match char with
            | 'y' ->
                yield! model |> handleAsyncResult (handler true)
            | 'n' ->
                let model = model |> MainModel.withMessage (MainStatus.CancelledConfirm confirmType)
                yield model
                yield! model |> handleAsyncResult (handler false)
            | _ -> ()
        | _ -> ()
    }

    let handleInputEvent (evt: InputEvent) (model: MainModel) =
        model.InputMode
        |> Option.map (fun mode ->
            match mode with
            | MarkPrompt (markType, markCommand) -> handleMarkPromptEvent markType markCommand
            | Confirm confirmType -> handleConfirmEvent confirmType
            | Input (Find multi) -> navigationHandler.HandleFindInputEvent multi
            | Input Search -> navigationHandler.HandleSearchInputEvent
            | Input NewFile
            | Input NewFolder -> itemActionHandler.HandleNewItemInputEvent (mode = Input NewFolder)
            | Input (Rename _) -> itemActionHandler.HandleRenameInputEvent
        )
        |> function
            | Some handler -> handler evt model
            | None -> AsyncSeq.empty

    let dispatcher evt =
        let handler =
            match evt with
            | KeyPress (chord, handler) -> Async (keyPress handleCommand chord handler.Handle)
            | ItemDoubleClick -> handleCommand (Navigation OpenCursorItem)
            | SettingsButtonClick -> handleCommand (Window OpenSettings)
            | LocationInputChanged -> Async navigationHandler.LocationInputChanged
            | LocationInputSubmit (path, handler) -> SyncResult (navigationHandler.LocationInputSubmit path handler)
            | LocationInputCancel -> Sync navigationHandler.LocationInputCancel
            | DeletePathSuggestion path -> Sync (navigationHandler.DeletePathSuggestion path)
            | InputEvent evt -> Async (handleInputEvent evt)
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
