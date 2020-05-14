namespace Koffee

open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Media
open System.ComponentModel
open System.Reactive.Subjects
open VinylUI
open VinylUI.Wpf
open Reflection
open Acadian.FSharp

module Obs = Observable

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

module MainView =
    let onKeyFunc key resultFunc (keyEvent : IEvent<KeyEventHandler, KeyEventArgs>) =
        keyEvent |> Obs.choose (fun evt ->
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

    let getPrompt pathFormat (item: Item) inputMode =
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
            sprintf "Permanently delete %s y/n ?" item.Description
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
            if window.ItemGrid.SelectedItem <> null then
                window.ItemGrid.ScrollIntoView(window.ItemGrid.SelectedItem)

        // setup grid
        let mutable relativePathFormat = string
        window.ItemGrid.AddColumn(<@ fun (i: Item) -> i.Type @>, "", conversion = (fun t -> t.Symbol))
        window.ItemGrid.AddColumn(<@ fun (i: Item) -> i.Name @>, widthWeight = 1.0)
        window.ItemGrid.AddColumn(<@ fun (i: Item) -> i.Path @>, "Relative Path", widthWeight = 1.0,
                                  conversion = (fun p -> p.Parent |> relativePathFormat))
        window.ItemGrid.AddColumn(<@ fun (i: Item) -> i.Modified @>, conversion = Option.toNullable,
                                  format = FormatString.dateTime)
        window.ItemGrid.AddColumn(<@ fun (i: Item) -> i.SizeFormatted @>, "Size", alignRight = true)
        window.ItemGrid.Columns |> Seq.iter (fun c -> c.CanUserSort <- false)
        let sortColumnsIndex =
            function
            | Type -> 0
            | Name -> 1
            | Modified -> 3
            | Size -> 4
        let setRelativePath relInfo =
            relativePathFormat <-
                match relInfo with
                | Some (path, fmt) -> (fun p -> p.FormatRelativeFolder fmt path)
                | None -> string
            window.ItemGrid.Columns.[2].Visibility <- if relInfo.IsSome then Visibility.Visible else Visibility.Collapsed

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
                    selectedPath |> Option.iter window.PathBox.set_Text
                window.PathBox.Select(window.PathBox.Text.Length, 0)
                e.Handled <- true
            | Key.Enter ->
                selectedPath |> Option.iter window.PathBox.set_Text
            | _ -> ()
        )
        window.PathBox.LostFocus.Add (fun _ -> window.PathSuggestions.Visible <- false)

        // bind Tab key to switch focus
        window.ItemGrid.PreviewKeyDown.Add (onKey Key.Tab (fun () ->
            window.PathBox.SelectAll()
            window.PathBox.Focus()
        ))

        // scroll path to show the end when it overflows
        window.PathBox.TextChanged.Add (fun _ ->
            if not window.PathBox.IsFocused then
                window.PathBox.ScrollToHorizontalOffset(window.PathBox.ActualWidth)
        )

        // on selection change, keep selected item in view
        window.ItemGrid.SelectedCellsChanged.Add (fun _ -> keepSelectedInView ())

        window.ItemGrid.SizeChanged.Add (fun _ -> keepSelectedInView ())

        window.InputBox.PreviewKeyDown.Add (onKey Key.Escape window.ItemGrid.Focus)
        window.InputBox.PreviewKeyDown.Add (fun e ->
            if window.SearchOptions.IsVisible then
                match e.Chord with
                | ModifierKeys.Control, Key.I -> window.SearchCaseSensitive.Toggle()
                | ModifierKeys.Control, Key.R -> window.SearchRegex.Toggle()
                | ModifierKeys.Control, Key.S -> window.SearchSubFolders.Toggle()
                | _ -> ()
        )

        if model.Config.Window.IsMaximized then
            window.WindowState <- WindowState.Maximized

        window.SettingsButton.Click.Add (fun _ -> window.ItemGrid.Focus() |> ignore)
        window.ItemGrid.Focus() |> ignore

        let version = typeof<MainModel>.Assembly.GetName().Version
        let versionStr = sprintf "%i.%i.%i" version.Major version.Minor version.Build

        // history save buffering
        let historyBuffer = new BehaviorSubject<History>(model.History)
        (historyBuffer |> Obs.throttle 3.0).Subscribe(history.set_Value) |> ignore
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

            Bind.modelMulti(<@ model.Items, model.Cursor, model.Sort @>).toFunc(fun (items, cursor, sort) ->
                if not <| obj.ReferenceEquals(window.ItemGrid.ItemsSource, items) then
                    window.ItemGrid.ItemsSource <- items
                window.ItemGrid.SelectedIndex <- cursor
                // sort indication
                let sortIndex, sortDir =
                    match sort with
                    | Some (field, desc) ->
                        let index = sortColumnsIndex field
                        let dir = if desc then ListSortDirection.Descending else ListSortDirection.Ascending
                        (Some index, Some dir)
                    | None -> (None, None)
                window.ItemGrid.Columns |> Seq.iteri (fun i c ->
                    c.SortDirection <- if Some i = sortIndex then sortDir |> Option.toNullable else Nullable()
                )
            )
            Bind.model(<@ model.Items @>).toFunc(fun items ->
                // directory status
                window.DirectoryStatus.Text <-
                    let fileSizes = items |> List.choose (fun n -> if n.Type = File then n.Size else None)
                    let fileStr =
                        match fileSizes with
                        | [] -> ""
                        | sizes -> sprintf ", %s" (sizes |> List.sum |> Format.fileSize)
                    sprintf "%i item%s%s" items.Length (if items.Length = 1 then "" else "s") fileStr
            )
            Bind.view(<@ window.ItemGrid.SelectedIndex @>).toModelOneWay(<@ model.Cursor @>)

            // display path
            Bind.model(<@ model.TitleLocation @>).toFunc(fun titleLoc ->
                window.Title <- sprintf "%s  |  Koffee v%s" titleLoc versionStr
            )

            // display yank register
            Bind.model(<@ model.Config.YankRegister @>).toFunc(fun register ->
                let text =
                    register |> Option.map (fun (path, typ, action) ->
                        sprintf "%A: %s %s" action typ.Symbol path.Name)
                window.RegisterText.Text <- text |? ""
                window.RegisterPanel.Visible <- text.IsSome
            )

            // update UI for input mode
            Bind.view(<@ window.InputBox.Text @>).toModel(<@ model.InputText @>, OnChange)
            Bind.view(<@ window.SearchCaseSensitive.IsChecked @>).toModel(<@ model.SearchCaseSensitive @>, ((=) (Nullable true)), Nullable)
            Bind.view(<@ window.SearchRegex.IsChecked @>).toModel(<@ model.SearchRegex @>, ((=) (Nullable true)), Nullable)
            Bind.view(<@ window.SearchSubFolders.IsChecked @>).toModel(<@ model.SearchSubFolders @>, ((=) (Nullable true)), Nullable)
            Bind.modelMulti(<@ model.InputMode, model.InputTextSelection, model.SelectedItem, model.PathFormat, model.Config.Bookmarks @>)
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
                        window.SearchOptions.Visibility <-
                            match inputMode with
                            | Input Search -> Visibility.Visible
                            | _ -> Visibility.Collapsed
                        window.InputText.Text <- getPrompt pathFormat selected inputMode
                        if not window.InputPanel.Visible then
                            window.InputPanel.Visible <- true
                            window.InputBox.Select(selectStart, selectLen)
                            window.InputBox.Focus() |> ignore
                    | None ->
                        if window.InputPanel.Visible then
                            window.InputPanel.Visibility <- Visibility.Collapsed
                            window.BookmarkPanel.Visible <- false
                            window.ItemGrid.Focus() |> ignore
                )
            Bind.model(<@ model.InputTextSelection @>).toFunc(fun (selectStart, selectLen) ->
                window.InputBox.Select(selectStart, selectLen)
            )
            Bind.modelMulti(<@ model.CurrentSearch, model.InputMode @>).toFunc(function
                | None, _
                | Some _, Some (Input Search) ->
                    window.SearchPanel.Visibility <- Visibility.Collapsed
                | Some (search, cs, re, sub), _ ->
                    window.SearchStatus.Text <- 
                        [   sprintf "Search results for \"%s\"" search
                            (if cs then "Case-sensitive" else "Not case-sensitive")
                            (if re then "Regular Expression" else "")
                            (if sub then "Sub-Folders" else "")
                        ] |> List.filter String.isNotEmpty |> String.concat ", "
                    window.SearchPanel.Visible <- true
            )
            Bind.modelMulti(<@ model.IsSearchingSubFolders, model.Location, model.PathFormat @>)
                .toFunc(fun (sub, loc, fmt) -> setRelativePath (if sub then Some (loc, fmt) else None))

            // update UI for status
            Bind.modelMulti(<@ model.Status, model.KeyCombo @>).toFunc(fun (status, keyCombo) ->
                window.StatusText.Text <- 
                    if not (keyCombo |> List.isEmpty) then
                        keyCombo
                        |> Seq.map KeyBinding.keyDescription
                        |> String.concat ""
                        |> sprintf "Pressed %s, waiting for another key..."
                    else
                        match status with
                        | Some (Message msg) | Some (ErrorMessage msg) | Some (Busy msg) -> msg
                        | None -> ""
                window.StatusText.Foreground <-
                    match keyCombo, status with
                    | [], Some (ErrorMessage _) -> Brushes.Red
                    | _ -> SystemColors.WindowTextBrush
                let isBusy =
                    match status with
                    | Some (Busy _) -> true
                    | _ -> false
                let wasBusy = not window.ItemGrid.IsEnabled
                window.PathBox.IsEnabled <- not isBusy
                window.ItemGrid.IsEnabled <- not isBusy
                window.Cursor <- if isBusy then Cursors.Wait else Cursors.Arrow
                if wasBusy && not isBusy then
                    window.ItemGrid.Focus() |> ignore
            )
            Bind.model(<@ model.Progress @>).toFunc(fun progress ->
                window.Progress.Value <- progress |? 0.0
                window.Progress.Visibility <- if progress.IsSome then Visibility.Visible else Visibility.Collapsed
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

    let events (config: ConfigFile) (history: HistoryFile) (subDirResults: IObservable<_>) (progress: IObservable<_>)
               (window: MainWindow) = [
        window.PathBox.PreviewKeyDown |> Obs.filter isNotModifier |> Obs.choose (fun evt ->
            let keyPress = KeyPress (evt.Chord, evt.Handler)
            let ignoreMods = [ ModifierKeys.None; ModifierKeys.Shift ]
            let ignoreCtrlKeys = [ Key.A; Key.Z; Key.X; Key.C; Key.V ]
            let focusGrid () = window.ItemGrid.Focus() |> ignore
            match evt.Chord with
            | (ModifierKeys.None, Key.Enter) -> Some (OpenPath (evt.HandlerWithEffect focusGrid))
            | (ModifierKeys.None, Key.Escape) -> focusGrid(); Some ResetLocationInput
            | (ModifierKeys.Control, key) when ignoreCtrlKeys |> List.contains key -> None
            | (modifier, _) when ignoreMods |> (not << List.contains modifier) -> Some keyPress
            | (_, key) when key >= Key.F1 && key <= Key.F12 -> Some keyPress
            | _ -> None
        )
        window.PathBox.TextChanged |> Obs.filter (fun _ -> window.PathBox.IsFocused)
                                   |> Obs.mapTo LocationInputChanged
        window.SettingsButton.Click |> Obs.mapTo OpenSettings

        window.ItemGrid.PreviewKeyDown |> Obs.filter isNotModifier
                                       |> Obs.map (fun evt -> KeyPress (evt.Chord, evt.Handler))
        window.ItemGrid.PreviewKeyDown |> Obs.choose (fun evt ->
            if evt.Chord = (ModifierKeys.Control, Key.C) then
                evt.Handled <- true // prevent Ctrl+C crash due to bug in WPF datagrid
            None
        )
        window.ItemGrid.MouseDoubleClick |> Obs.mapTo OpenSelected
        window.ItemGrid.SizeChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.choose (fun _ ->
            let grid = window.ItemGrid
            if grid.HasItems then
                let index = grid.SelectedIndex |> max 0
                grid.ItemContainerGenerator.ContainerFromIndex(index) :?> DataGridRow
                |> Option.ofObj
                |> Option.map (fun row -> grid.ActualHeight / row.ActualHeight |> int |> PageSizeChanged)
            else None
        )

        window.InputBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> SubmitInput)
        window.InputBox.PreviewKeyDown |> Obs.choose (fun keyEvt ->
            match keyEvt.Key with
            | Key.Up -> Some InputBack
            | Key.Down -> Some InputForward
            | Key.Delete -> Some (InputDelete keyEvt.Handler)
            | _ -> None
        )
        window.InputBox.PreviewTextInput |> Obs.choose (fun keyEvt ->
            match keyEvt.Text.ToCharArray() with
            | [| c |] -> Some (InputCharTyped (c, keyEvt.Handler))
            | _ -> None
        )
        window.InputBox.TextChanged |> Obs.mapTo InputChanged
        window.SearchCaseSensitive.CheckedChanged |> Obs.mapTo InputChanged
        window.SearchRegex.CheckedChanged |> Obs.mapTo InputChanged
        window.SearchSubFolders.CheckedChanged |> Obs.mapTo InputChanged
        subDirResults |> Obs.buffer 0.3
                      |> Obs.filter Seq.isNotEmpty
                      |> Obs.onCurrent
                      |> Obs.map (List.concat >> SubDirectoryResults)
        window.InputBox.LostFocus |> Obs.mapTo CancelInput
        progress |> Obs.buffer 0.3
                 |> Obs.filter Seq.isNotEmpty
                 |> Obs.onCurrent
                 |> Obs.map (Seq.reduce (Option.map2 (+)) >> AddProgress)

        window.Activated |> Obs.filter (fun _ -> window.IsLoaded) |> Obs.mapTo WindowActivated
        window.LocationChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.choose (fun _ ->
            if window.Left > -window.Width && window.Top > -window.Height then
                Some (WindowLocationChanged (int window.Left, int window.Top))
            else None
        )
        window.SizeChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.map (fun _ ->
            WindowSizeChanged (int window.Width, int window.Height)
        )
        window.StateChanged |> Obs.choose (fun _ ->
            if window.WindowState <> WindowState.Minimized then
                Some (WindowMaximizedChanged (window.WindowState = WindowState.Maximized))
            else None
        )
        config.FileChanged |> Obs.onCurrent |> Obs.map ConfigFileChanged
        history.FileChanged |> Obs.onCurrent |> Obs.map HistoryFileChanged
    ]

module MainStatus =
    // navigation
    let find prefix = Message <| "Find item starting with: " + prefix
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
        | PutItem (Move, item, newPath) -> Some <| sprintf "Moving %s to \"%s\"..." item.Description (newPath.Format pathFormat)
        | PutItem (Copy, item, newPath) -> Some <| sprintf "Copying %s to \"%s\"..." item.Description (newPath.Format pathFormat)
        | DeletedItem (item, false) -> Some <| sprintf "Recycling %s..." item.Description
        | DeletedItem (item, true) -> Some <| sprintf "Deleting %s..." item.Description
        | _ -> None
    let runningAction action pathFormat =
        runningActionMessage action pathFormat |> Option.map Busy
    let checkingIsRecyclable = Busy <| "Calculating size..."
    let private actionCompleteMessage action pathFormat =
        match action with
        | CreatedItem item -> sprintf "Created %s" item.Description
        | RenamedItem (item, newName) -> sprintf "Renamed %s to \"%s\"" item.Description newName
        | PutItem (Move, item, newPath) -> sprintf "Moved %s to \"%s\"" item.Description (newPath.Format pathFormat)
        | PutItem (Copy, item, newPath) -> sprintf "Copied %s to \"%s\"" item.Description (newPath.Format pathFormat)
        | PutItem (Shortcut, item, _) -> sprintf "Created shortcut to %s \"%s\""
                                                 (item.Type |> string |> String.toLower) (item.Path.Format pathFormat)
        | DeletedItem (item, false) -> sprintf "Sent %s to Recycle Bin" item.Description
        | DeletedItem (item, true) -> sprintf "Deleted %s" item.Description
    let actionComplete action pathFormat =
        actionCompleteMessage action pathFormat |> Message

    let cancelled = Message <| "Cancelled"

    // undo/redo
    let undoingCreate (item: Item) = Busy <| sprintf "Undoing creation of %s - Deleting..." item.Description
    let undoingMove (item: Item) = Busy <| sprintf "Undoing move of %s..." item.Description
    let undoingCopy (item: Item) isDeletionPermanent =
        let undoVerb = if isDeletionPermanent then "Deleting" else "Recycling"
        Busy <| sprintf "Undoing copy of %s - %s..." item.Description undoVerb
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
    | CannotUseNameAlreadyExists of actionName: string * itemType: ItemType * name: string * hidden: bool
    | CannotMoveToSameFolder
    | TooManyCopies of fileName: string
    | CannotUndoNonEmptyCreated of Item
    | CannotUndoMoveToExisting of moded: Item
    | CannotUndoDelete of permanent: bool * item: Item
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
                | CreatedItem item -> sprintf "create %s" item.Description
                | RenamedItem (item, newName) -> sprintf "rename %s" item.Description
                | PutItem (action, item, newPath) ->
                    let action = action |> string |> String.toLower
                    sprintf "%s %s to \"%s\"" action item.Description (newPath.Format pathFormat)
                | DeletedItem (item, false) -> sprintf "recycle %s" item.Description
                | DeletedItem (item, true) -> sprintf "delete %s" item.Description
            (ActionError (actionName, e)).Message
        | InvalidPath path -> "Path format is invalid: " + path
        | ShortcutTargetMissing path -> "Shortcut target does not exist: " + path
        | InvalidSearchSlash -> "Invalid search: only one slash \"/\" may be used. Slash is used to delimit switches."
        | InvalidSearchSwitch c -> sprintf "Invalid search switch \"%c\". Valid switches are: c, i" c
        | YankRegisterItemMissing path -> "Item in yank register no longer exists: " + path
        | CannotPutHere -> "Cannot put items here"
        | CannotUseNameAlreadyExists (actionName, itemType, name, hidden) ->
            let append = if hidden then " (hidden)" else ""
            sprintf "Cannot %s %O \"%s\" because an item with that name already exists%s"
                    actionName itemType name append
        | CannotMoveToSameFolder -> "Cannot move item to same folder it is already in"
        | TooManyCopies fileName -> sprintf "There are already too many copies of \"%s\"" fileName
        | CannotUndoNonEmptyCreated item ->
            sprintf "Cannot undo creation of %s because it is no longer empty" item.Description
        | CannotUndoMoveToExisting moved -> sprintf "Cannot undo move of %s because an item exists in its previous location" moved.Name
        | CannotUndoDelete (permanent, item) ->
            if permanent then
                sprintf "Cannot undo deletion of %s" item.Description
            else
                sprintf "Cannot undo recycling of %s. Please open the Recycle Bin in Windows Explorer to restore this item" item.Description
        | NoUndoActions -> "No more actions to undo"
        | NoRedoActions -> "No more actions to redo"
        | CouldNotOpenApp (app, e) -> sprintf "Could not open app %s: %s" app e.Message
        | CouldNotFindKoffeeExe -> "Could not determine Koffee.exe path"

[<AutoOpen>]
module MainModelExt =
    type MainModel with
        member this.WithError (e: MainError) =
            { this with Status = Some (ErrorMessage e.Message) }
