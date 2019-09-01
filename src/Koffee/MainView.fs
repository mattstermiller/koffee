namespace Koffee

open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Media
open System.ComponentModel
open System.Reactive.Linq
open System.Reactive.Concurrency
open System.Reactive.Subjects
open VinylUI
open VinylUI.Wpf
open Reflection
open Acadian.FSharp

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

module MainView =
    let onKeyFunc key resultFunc (keyEvent : IEvent<KeyEventHandler, KeyEventArgs>) =
        keyEvent |> Observable.choose (fun evt ->
            if evt.Key = key then
                evt.Handled <- true
                Some <| resultFunc()
            else
                None)

    let isNotModifier (evt: KeyEventArgs) =
        let modifierKeys = [
            Key.LeftShift; Key.RightShift; Key.LeftCtrl; Key.RightCtrl;
            Key.LeftAlt; Key.RightAlt; Key.LWin; Key.RWin; Key.System
        ]
        not <| List.contains evt.RealKey modifierKeys

    let throttleChanges o =
        Observable.Throttle(o, TimeSpan.FromSeconds(0.5)).ObserveOn(DispatcherScheduler.Current)

    let getPrompt pathFormat (node: Node) inputMode =
        let caseName (case: obj) = case |> GetUnionCaseName |> String.readableIdentifier |> sprintf "%s:"
        match inputMode with
        | Confirm (Overwrite (putAction, src, dest)) ->
            match dest.Type with
            | Folder ->
                sprintf "Folder \"%s\" already exists. %A anyway and merge files y/n ?" dest.Name putAction
            | File ->
                match src.Modified, src.Size, dest.Modified, dest.Size with
                | Some srcModified, Some srcSize, Some destModified, Some destSize ->
                    let compare a b less greater =
                        if a = b then "same"
                        else if a < b then less
                        else greater
                    sprintf "File \"%s\" already exists. Overwrite with file dated %s (%s), size %s (%s) y/n ?"
                        dest.Name
                        (Format.dateTime srcModified) (compare srcModified destModified "older" "newer")
                        (Format.fileSize srcSize) (compare srcSize destSize "smaller" "larger")
                | _ -> sprintf "File \"%s\" already exists. Overwrite it y/n ?" dest.Name
            | _ -> ""
        | Confirm Delete ->
            sprintf "Permanently delete %s y/n ?" node.Description
        | Confirm (OverwriteBookmark (char, existingPath)) ->
            sprintf "Overwrite bookmark \"%c\" currently set to \"%s\" y/n ?" char (existingPath.Format pathFormat)
        | Prompt promptType ->
            promptType |> caseName
        | Input (Find multi) ->
            sprintf "Find item starting with%s:" (if multi then " (multi)" else "")
        | Input inputType ->
            inputType |> caseName

    let binder (config: ConfigFile) (history: HistoryFile) (window: MainWindow) model =
        let keepSelectedInView () =
            if window.NodeGrid.SelectedItem <> null then
                window.NodeGrid.ScrollIntoView(window.NodeGrid.SelectedItem)

        // setup grid
        window.NodeGrid.AddColumn("DisplayName", "Name", widthWeight = 3.0)
        window.NodeGrid.AddColumn("Type")
        window.NodeGrid.AddColumn("Modified", converter = ValueConverters.OptionValue(), format = FormatString.dateTime)
        window.NodeGrid.AddColumn("SizeFormatted", "Size", alignRight = true)
        window.NodeGrid.Columns |> Seq.iter (fun c -> c.CanUserSort <- false)

        // path suggestions
        window.PathBox.PreviewKeyDown.Add (fun e ->
            let paths = window.PathSuggestions
            let items = paths.Items.Count
            let selectedPath = paths.SelectedItem |> unbox |> Option.ofString
            match e.Key with
            | Key.Up when paths.IsEnabled && items > 0 ->
                paths.SelectedIndex <- if paths.SelectedIndex > 0 then paths.SelectedIndex - 1 else items - 1
                e.Handled <- true
            | Key.Down when paths.IsEnabled && items > 0 ->
                paths.SelectedIndex <- if paths.SelectedIndex < items - 1 then paths.SelectedIndex + 1 else 0
                e.Handled <- true
            | Key.Tab ->
                if paths.Visible then
                    selectedPath |> Option.iter (fun path ->
                        window.PathBox.Text <- path
                        window.PathBox.Select(path.Length, 0)
                    )
                e.Handled <- true
            | Key.Enter ->
                selectedPath |> Option.iter window.PathBox.set_Text
            | _ -> ()
        )
        window.PathBox.LostFocus.Add (fun _ -> window.PathSuggestions.Visible <- false)

        // bind Tab key to switch focus
        window.PathBox.PreviewKeyDown.Add (onKey Key.Escape (fun () ->
            if window.PathBox.SelectionLength > 0 then
                window.PathBox.Select(window.PathBox.SelectionStart + window.PathBox.SelectionLength, 0)
            else
                window.NodeGrid.Focus() |> ignore
        ))
        window.NodeGrid.PreviewKeyDown.Add (onKey Key.Tab (fun () ->
            window.PathBox.SelectAll()
            window.PathBox.Focus()
        ))

        // scroll path to show the end when it overflows
        window.PathBox.TextChanged.Add (fun _ ->
            if not window.PathBox.IsFocused then
                window.PathBox.ScrollToHorizontalOffset(window.PathBox.ActualWidth)
        )

        // on selection change, keep selected node in view
        window.NodeGrid.SelectedCellsChanged.Add (fun _ -> keepSelectedInView ())

        window.NodeGrid.SizeChanged.Add (fun _ -> keepSelectedInView ())

        window.InputBox.PreviewKeyDown.Add (onKey Key.Escape window.NodeGrid.Focus)

        if model.Config.Window.IsMaximized then
            window.WindowState <- WindowState.Maximized

        window.SettingsButton.Click.Add (fun _ -> window.NodeGrid.Focus() |> ignore)
        window.NodeGrid.Focus() |> ignore

        let version = typeof<MainModel>.Assembly.GetName().Version
        let versionStr = sprintf "%i.%i.%i" version.Major version.Minor version.Build

        // history save buffering
        let historyBuffer = new BehaviorSubject<History>(model.History)
        historyBuffer.Throttle(TimeSpan.FromSeconds(3.0)).Subscribe(history.set_Value) |> ignore
        window.Closed.Add (fun _ ->
            history.Value <- historyBuffer.Value
            historyBuffer.Dispose()
        )

        [   Bind.view(<@ window.PathBox.Text @>).toModel(<@ model.LocationInput @>, OnChange)
            Bind.model(<@ model.PathSuggestions @>).toFunc(function
                | Ok paths ->
                    window.PathSuggestions.ItemsSource <- paths
                    window.PathSuggestions.SelectedIndex <- if paths.Length = 1 then 0 else -1
                    window.PathSuggestions.IsEnabled <- true
                    window.PathSuggestions.Visible <- window.PathBox.IsFocused && not paths.IsEmpty
                | Error error ->
                    window.PathSuggestions.ItemsSource <- ["Error: " + error]
                    window.PathSuggestions.IsEnabled <- false
                    window.PathSuggestions.Visible <- window.PathBox.IsFocused
            )

            Bind.modelMulti(<@ model.Nodes, model.Cursor, model.Sort @>).toFunc(fun (nodes, cursor, (sortField, sortDesc)) ->
                if not <| obj.ReferenceEquals(window.NodeGrid.ItemsSource, nodes) then
                    window.NodeGrid.ItemsSource <- nodes
                window.NodeGrid.SelectedIndex <- cursor
                // sort indication
                let sortDir =
                    if sortDesc then ListSortDirection.Descending
                    else ListSortDirection.Ascending
                let sortColumnIndex =
                    match sortField with
                    | Name -> 0
                    | Type -> 1
                    | Modified -> 2
                    | Size -> 3
                window.NodeGrid.Columns.[sortColumnIndex].SortDirection <- Nullable sortDir
            )
            Bind.view(<@ window.NodeGrid.SelectedIndex @>).toModelOneWay(<@ model.Cursor @>)

            // display path
            Bind.model(<@ model.TitleLocation @>).toFunc(fun titleLoc ->
                window.Title <- sprintf "%s  |  Koffee v%s" titleLoc versionStr
            )

            // display yank register
            Bind.model(<@ model.Config.YankRegister @>).toFunc(fun register ->
                let text =
                    register |> Option.map (fun (path, typ, action) ->
                        sprintf "%A %A: %s" action typ path.Name)
                window.RegisterText.Text <- text |? ""
                window.RegisterPanel.Visible <- text.IsSome
            )

            // update UI for input mode
            Bind.view(<@ window.InputBox.Text @>).toModel(<@ model.InputText @>, OnChange)
            Bind.modelMulti(<@ model.InputMode, model.InputTextSelection, model.SelectedNode, model.PathFormat, model.Config.Bookmarks @>)
                .toFunc(fun (inputMode, (selectStart, selectLen), selected, pathFormat, bookmarks) ->
                    match inputMode with
                    | Some inputMode ->
                        match inputMode with
                        | Prompt GoToBookmark
                        | Prompt SetBookmark
                        | Prompt DeleteBookmark ->
                            let bookmarks =
                                bookmarks
                                |> List.map (fun (c, p) -> (c, p.Format pathFormat))
                                |> Seq.ifEmpty [(' ', "No bookmarks set")]
                            window.Bookmarks.ItemsSource <- bookmarks
                            window.BookmarkPanel.Visible <- true
                        | _ ->
                            window.BookmarkPanel.Visible <- false
                        window.InputText.Text <- getPrompt pathFormat selected inputMode
                        if not window.InputPanel.Visible then
                            window.InputPanel.Visible <- true
                            window.InputBox.Select(selectStart, selectLen)
                            window.InputBox.Focus() |> ignore
                    | None ->
                        if window.InputPanel.Visible then
                            window.InputPanel.Visible <- false
                            window.BookmarkPanel.Visible <- false
                            window.NodeGrid.Focus() |> ignore
                )

            // update UI for status
            Bind.modelMulti(<@ model.Status, model.KeyCombo, model.Nodes @>).toFunc(fun (status, keyCombo, nodes) ->
                window.StatusText.Text <- 
                    if not (keyCombo |> List.isEmpty) then
                        keyCombo
                        |> Seq.map KeyBinding.keyDescription
                        |> String.concat ""
                        |> sprintf "Pressed %s, waiting for another key..."
                    else
                        match status with
                        | Some (Message msg) | Some (ErrorMessage msg) | Some (Busy msg) -> msg
                        | None ->
                            let fileSizes = nodes |> List.choose (fun n -> if n.Type = File then n.Size else None)
                            let fileStr =
                                match fileSizes with
                                | [] -> ""
                                | sizes -> sprintf ", %s" (sizes |> List.sum |> Format.fileSize)
                            sprintf "%i items%s" nodes.Length fileStr
                window.StatusText.Foreground <-
                    match keyCombo, status with
                    | [], Some (ErrorMessage _) -> Brushes.Red
                    | _ -> SystemColors.WindowTextBrush
                let isBusy =
                    match status with
                    | Some (Busy _) -> true
                    | _ -> false
                let wasBusy = not window.NodeGrid.IsEnabled
                window.PathBox.IsEnabled <- not isBusy
                window.NodeGrid.IsEnabled <- not isBusy
                window.Cursor <- if isBusy then Cursors.Wait else Cursors.Arrow
                if wasBusy && not isBusy then
                    window.NodeGrid.Focus() |> ignore
            )

            Bind.model(<@ model.WindowLocation @>).toFunc(fun (left, top) ->
                if int window.Left <> left then window.Left <- float left
                if int window.Top <> top then window.Top <- float top
            )
            Bind.model(<@ model.WindowSize @>).toFunc(fun (width, height) ->
                if int window.Width <> width then window.Width <- float width
                if int window.Height <> height then window.Height <- float height
            )

            Bind.model(<@ model.Config @>).toFunc(config.set_Value)
            Bind.model(<@ model.History @>).toFunc(historyBuffer.OnNext)
        ]

    let events (config: ConfigFile) (history: HistoryFile) (window: MainWindow) = [
        window.PathBox.PreviewKeyDown |> Observable.filter isNotModifier |> Observable.choose (fun evt ->
            let keyPress = KeyPress (evt.Chord, evt.Handler)
            let ignoreMods = [ ModifierKeys.None; ModifierKeys.Shift ]
            let ignoreCtrlKeys = [ Key.A; Key.Z; Key.X; Key.C; Key.V ]
            let focusGrid () = window.NodeGrid.Focus() |> ignore
            match evt.Chord with
            | (ModifierKeys.None, Key.Enter) -> Some (OpenPath (evt.HandlerWithEffect focusGrid))
            | (ModifierKeys.Control, key) when ignoreCtrlKeys |> List.contains key -> None
            | (modifier, _) when ignoreMods |> (not << List.contains modifier) -> Some keyPress
            | (_, key) when key >= Key.F1 && key <= Key.F12 -> Some keyPress
            | _ -> None
        )
        window.PathBox.TextChanged |> Observable.filter (fun _ -> window.PathBox.IsFocused)
                                   |> Observable.mapTo PathInputChanged
        window.SettingsButton.Click |> Observable.mapTo OpenSettings

        window.NodeGrid.PreviewKeyDown |> Observable.filter isNotModifier
                                       |> Observable.map (fun evt -> KeyPress (evt.Chord, evt.Handler))
        window.NodeGrid.PreviewKeyDown |> Observable.choose (fun evt ->
            if evt.Chord = (ModifierKeys.Control, Key.C) then
                evt.Handled <- true // prevent Ctrl+C crash due to bug in WPF datagrid
            None
        )
        window.NodeGrid.MouseDoubleClick |> Observable.mapTo OpenSelected
        window.NodeGrid.SizeChanged |> throttleChanges |> Observable.choose (fun _ ->
            let grid = window.NodeGrid
            if grid.HasItems then
                let index = grid.SelectedIndex |> max 0
                grid.ItemContainerGenerator.ContainerFromIndex(index) :?> DataGridRow
                |> Option.ofObj
                |> Option.map (fun row -> grid.ActualHeight / row.ActualHeight |> int |> PageSizeChanged)
            else None
        )

        window.InputBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> SubmitInput)
        window.InputBox.PreviewKeyDown |> Observable.choose (fun keyEvt ->
            if keyEvt.Key = Key.Delete then
                Some (InputDelete keyEvt.Handler)
            else None
        )
        window.InputBox.PreviewTextInput |> Observable.choose (fun keyEvt ->
            match keyEvt.Text.ToCharArray() with
            | [| c |] -> Some (InputCharTyped (c, keyEvt.Handler))
            | _ -> None
        )
        window.InputBox.TextChanged |> Observable.mapTo InputChanged
        window.InputBox.LostFocus |> Observable.mapTo CancelInput

        window.Activated |> Observable.choose (fun _ ->
            if config.Value.Window.RefreshOnActivate && window.IsLoaded then
                Some Refresh
            else None
        )
        window.LocationChanged |> throttleChanges |> Observable.choose (fun _ ->
            if window.Left > -window.Width && window.Top > -window.Height then
                Some (WindowLocationChanged (int window.Left, int window.Top))
            else None
        )
        window.SizeChanged |> throttleChanges |> Observable.map (fun _ ->
            WindowSizeChanged (int window.Width, int window.Height)
        )
        window.StateChanged |> Observable.choose (fun _ ->
            if window.WindowState <> WindowState.Minimized then
                Some (WindowMaximizedChanged (window.WindowState = WindowState.Maximized))
            else None
        )
        config.FileChanged.ObserveOn(DispatcherScheduler.Current) |> Observable.map ConfigFileChanged
        history.FileChanged.ObserveOn(DispatcherScheduler.Current) |> Observable.map HistoryFileChanged
    ]

module MainStatus =
    // navigation
    let find prefix = Message <| "Find item starting with: " + prefix
    let search matches caseSensitive searchStr =
        let cs = if caseSensitive then " (case-sensitive)" else ""
        Message <| sprintf "Search \"%s\"%s found %i matches" searchStr cs matches
    let noBookmark char = Message <| sprintf "Bookmark \"%c\" not set" char
    let setBookmark char path = Message <| sprintf "Set bookmark \"%c\" to %s" char path
    let deletedBookmark char path = Message <| sprintf "Deleted bookmark \"%c\" that was set to %s" char path

    // actions
    let sort field desc = Message <| sprintf "Sort by %A %s" field (if desc then "descending" else "ascending")
    let toggleHidden showing = Message <| sprintf "%s hidden files" (if showing then "Showing" else "Hiding")
    let openFile name = Message <| sprintf "Opened File: %s" name
    let openProperties name = Message <| sprintf "Opened Properties: %s" name
    let openExplorer = Message "Opened Windows Explorer"
    let openCommandLine path = Message <| sprintf "Opened Commandline at: %s" path
    let openTextEditor name = Message <| sprintf "Opened text editor for: %s" name
    let clipboardCopy path = Message <| sprintf "Copied to clipboard: %s" path
    let removedNetworkHost host = Message <| sprintf "Removed network host: %s" host

    let private runningActionMessage action pathFormat =
        match action with
        | PutItem (Move, node, newPath) -> Some <| sprintf "Moving %s to \"%s\"..." node.Description (newPath.Format pathFormat)
        | PutItem (Copy, node, newPath) -> Some <| sprintf "Copying %s to \"%s\"..." node.Description (newPath.Format pathFormat)
        | DeletedItem (node, false) -> Some <| sprintf "Recycling %s..." node.Description
        | DeletedItem (node, true) -> Some <| sprintf "Deleting %s..." node.Description
        | _ -> None
    let runningAction action pathFormat =
        runningActionMessage action pathFormat |> Option.map Busy
    let checkingIsRecyclable = Busy <| "Calculating size..."
    let private actionCompleteMessage action pathFormat =
        match action with
        | CreatedItem node -> sprintf "Created %s" node.Description
        | RenamedItem (node, newName) -> sprintf "Renamed %s to \"%s\"" node.Description newName
        | PutItem (Move, node, newPath) -> sprintf "Moved %s to \"%s\"" node.Description (newPath.Format pathFormat)
        | PutItem (Copy, node, newPath) -> sprintf "Copied %s to \"%s\"" node.Description (newPath.Format pathFormat)
        | PutItem (Shortcut, node, _) -> sprintf "Created shortcut to %s \"%s\""
                                                 (node.Type |> string |> String.toLower) (node.Path.Format pathFormat)
        | DeletedItem (node, false) -> sprintf "Sent %s to Recycle Bin" node.Description
        | DeletedItem (node, true) -> sprintf "Deleted %s" node.Description
    let actionComplete action pathFormat =
        actionCompleteMessage action pathFormat |> Message

    let cancelled = Message <| "Cancelled"

    // undo/redo
    let undoingCreate (node: Node) = Busy <| sprintf "Undoing creation of %s - Deleting..." node.Description
    let undoingMove (node: Node) = Busy <| sprintf "Undoing move of %s..." node.Description
    let undoingCopy (node: Node) isDeletionPermanent =
        let undoVerb = if isDeletionPermanent then "Deleting" else "Recycling"
        Busy <| sprintf "Undoing copy of %s - %s..." node.Description undoVerb
    let undoAction action pathFormat =
        Message <| (actionCompleteMessage action pathFormat |> sprintf "Action undone: %s")

    let redoingAction action pathFormat =
        runningActionMessage action pathFormat
            |> Option.map (fun m -> Busy <| sprintf "Redoing action: %s" m)
    let redoAction action pathFormat =
        Message <| (actionCompleteMessage action pathFormat |> sprintf "Action redone: %s")


type MainError =
    | ActionError of actionName: string * exn
    | ItemActionError of ItemAction * PathFormat * exn
    | InvalidPath of string
    | ShortcutTargetMissing of string
    | InvalidSearchSlash
    | InvalidSearchSwitch of char
    | YankRegisterItemMissing of string
    | CannotPutHere
    | CannotUseNameAlreadyExists of actionName: string * nodeType: NodeType * name: string * hidden: bool
    | CannotMoveToSameFolder
    | TooManyCopies of fileName: string
    | CannotUndoNonEmptyCreated of Node
    | CannotUndoMoveToExisting of moded: Node
    | CannotUndoDelete of permanent: bool * node: Node
    | NoUndoActions
    | NoRedoActions
    | CouldNotOpenApp of app: string * exn
    | CouldNotFindKoffeeExe

    member this.Message =
        match this with
        | ActionError (action, e) ->
            let msg =
                match e with
                | :? AggregateException as agg -> agg.InnerExceptions.[0].Message
                | e -> e.Message
            sprintf "Could not %s: %s" action msg
        | ItemActionError (action, pathFormat, e) ->
            let actionName =
                match action with
                | CreatedItem node -> sprintf "create %s" node.Description
                | RenamedItem (node, newName) -> sprintf "rename %s" node.Description
                | PutItem (action, node, newPath) ->
                    let action = action |> string |> String.toLower
                    sprintf "%s %s to \"%s\"" action node.Description (newPath.Format pathFormat)
                | DeletedItem (node, false) -> sprintf "recycle %s" node.Description
                | DeletedItem (node, true) -> sprintf "delete %s" node.Description
            (ActionError (actionName, e)).Message
        | InvalidPath path -> "Path format is invalid: " + path
        | ShortcutTargetMissing path -> "Shortcut target does not exist: " + path
        | InvalidSearchSlash -> "Invalid search: only one slash \"/\" may be used. Slash is used to delimit switches."
        | InvalidSearchSwitch c -> sprintf "Invalid search switch \"%c\". Valid switches are: c, i" c
        | YankRegisterItemMissing path -> "Item in yank register no longer exists: " + path
        | CannotPutHere -> "Cannot put items here"
        | CannotUseNameAlreadyExists (actionName, nodeType, name, hidden) ->
            let append = if hidden then " (hidden)" else ""
            sprintf "Cannot %s %O \"%s\" because an item with that name already exists%s"
                    actionName nodeType name append
        | CannotMoveToSameFolder -> "Cannot move item to same folder it is already in"
        | TooManyCopies fileName -> sprintf "There are already too many copies of \"%s\"" fileName
        | CannotUndoNonEmptyCreated node ->
            sprintf "Cannot undo creation of %s because it is no longer empty" node.Description
        | CannotUndoMoveToExisting moved -> sprintf "Cannot undo move of %s because an item exists in its previous location" moved.Name
        | CannotUndoDelete (permanent, node) ->
            if permanent then
                sprintf "Cannot undo deletion of %s" node.Description
            else
                sprintf "Cannot undo recycling of %s. Please open the Recycle Bin in Windows Explorer to restore this item" node.Description
        | NoUndoActions -> "No more actions to undo"
        | NoRedoActions -> "No more actions to redo"
        | CouldNotOpenApp (app, e) -> sprintf "Could not open app %s: %s" app e.Message
        | CouldNotFindKoffeeExe -> "Could not determine Koffee.exe path"

[<AutoOpen>]
module MainModelExt =
    type MainModel with
        member this.WithError (e: MainError) =
            { this with Status = Some (ErrorMessage e.Message) }
