namespace Koffee

open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.ComponentModel
open System.Reactive.Subjects
open VinylUI
open VinylUI.Wpf
open Reflection
open Acadian.FSharp
open KoffeeUI

module Obs = Observable

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
        | Confirm (OverwriteSavedSearch (char, existingSearch)) ->
            sprintf "Overwrite saved search \"%c\" currently set to \"%s\" y/n ?" char (string existingSearch)
        | Prompt promptType ->
            promptType |> caseName
        | Input (Find multi) ->
            sprintf "Find item starting with%s:" (if multi then " (multi)" else "")
        | Input inputType ->
            let symbol =
                match inputType with
                | CreateFile -> File.Symbol + " "
                | CreateFolder -> Folder.Symbol + " "
                | _ -> ""
            symbol + (inputType |> caseName)

    /// trim lists to the given stack size, but if a stack is smaller, allow other stack to grow up to stackSize*2
    let trimStacks stackSize prev next =
        let gap l = max 0 (stackSize - List.length l)
        let prev = prev |> List.truncate (stackSize + gap next)
        let next = next |> List.truncate (stackSize + gap prev)
        (prev, next)

    let binder (config: ConfigFile) (history: HistoryFile) (progress: IObservable<_>) (window: MainWindow) model =
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
            | _ -> ()
        )
        window.PathBox.LostFocus.Add (fun _ -> window.PathSuggestions.Visible <- false)

        // scroll path to show the end when it overflows
        window.PathBox.TextChanged.Add (fun _ ->
            if not window.PathBox.IsFocused then
                window.PathBox.ScrollToHorizontalOffset(window.PathBox.ActualWidth)
        )

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
            window.ItemGrid.Columns.[2].Collapsed <- relInfo.IsNone

        // bind Tab key to switch focus
        window.ItemGrid.PreviewKeyDown.Add (onKey Key.Tab (fun () ->
            window.PathBox.SelectAll()
            window.PathBox.Focus()
        ))

        // on selection change, keep selected item in view
        let keepSelectedInView _ =
            if window.ItemGrid.SelectedItem <> null then
                window.ItemGrid.ScrollIntoView(window.ItemGrid.SelectedItem)
        window.ItemGrid.SelectedCellsChanged.Add keepSelectedInView
        window.ItemGrid.SizeChanged.Add keepSelectedInView

        // keep grid in focus when user clicks a cell to prevent focus issue
        window.ItemGrid.SelectionChanged.Add (fun e ->
            if FocusManager.GetFocusedElement(window) :? DataGridCell then
                window.ItemGrid.Focus() |> ignore
        )

        window.InputBox.PreviewKeyDown.Add (onKey Key.Escape window.ItemGrid.Focus)
        window.InputBox.PreviewKeyDown.Add (fun e ->
            if window.SearchOptions.IsVisible then
                match e.Chord with
                | ModifierKeys.Control, Key.I -> window.SearchCaseSensitive.Toggle()
                | ModifierKeys.Control, Key.R -> window.SearchRegex.Toggle()
                | ModifierKeys.Control, Key.S -> window.SearchSubFolders.Toggle()
                | _ -> ()
        )

        window.Progress.Collapsed <- true
        progress
            |> Obs.buffer 0.3
            |> Obs.onCurrent
            |> Obs.map (Seq.reduce (fun p1 p2 ->
                match p1, p2 with
                | Some p1, Some p2 -> Some (p1 + p2)
                | None, Some p2 -> Some p2
                | _ -> None
            ))
            |> Obs.add (fun incr ->
                window.Progress.Value <- incr |> Option.map ((+) window.Progress.Value) |? 0.0
                window.Progress.Collapsed <- incr.IsNone
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

        // bindings
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
            Bind.view(<@ window.SearchCaseSensitive.IsChecked @>).toModel(<@ model.SearchInput.CaseSensitive @>, ((=) (Nullable true)), Nullable)
            Bind.view(<@ window.SearchRegex.IsChecked @>).toModel(<@ model.SearchInput.Regex @>, ((=) (Nullable true)), Nullable)
            Bind.view(<@ window.SearchSubFolders.IsChecked @>).toModel(<@ model.SearchInput.SubFolders @>, ((=) (Nullable true)), Nullable)
            Bind.modelMulti(<@ model.InputMode, model.InputTextSelection, model.SelectedItem, model.PathFormat, model.Config.Bookmarks, model.Config.SavedSearches @>)
                .toFunc(fun (inputMode, (selectStart, selectLen), selected, pathFormat, bookmarks, searches) ->
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
                            window.BookmarksHeader.Text <- "Bookmarks"
                            window.BookmarkPanel.Visible <- true
                        | Prompt GoToSavedSearch
                        | Prompt SetSavedSearch
                        | Prompt DeleteSavedSearch ->
                            let searches =
                                searches
                                |> List.map (sndf string)
                                |> Seq.ifEmpty [(' ', "No searches saved")]
                            window.Bookmarks.ItemsSource <- searches
                            window.BookmarksHeader.Text <- "Saved Searches"
                            window.BookmarkPanel.Visible <- true
                        | _ ->
                            window.BookmarkPanel.Visible <- false
                        window.SearchOptions.Collapsed <- inputMode <> Input Search
                        window.InputText.Text <- getPrompt pathFormat selected inputMode
                        if not window.InputPanel.Visible then
                            window.InputPanel.Visible <- true
                            window.InputBox.Select(selectStart, selectLen)
                            window.InputBox.Focus() |> ignore
                    | None ->
                        if window.InputPanel.Visible then
                            window.InputPanel.Collapsed <- true
                            window.BookmarkPanel.Visible <- false
                            window.ItemGrid.Focus() |> ignore
                )
            Bind.model(<@ model.InputTextSelection @>).toFunc(fun (selectStart, selectLen) ->
                window.InputBox.Select(selectStart, selectLen)
            )
            Bind.modelMulti(<@ model.SearchCurrent, model.InputMode @>).toFunc(function
                | None, _
                | Some _, Some (Input Search) ->
                    window.SearchPanel.Collapsed <- true
                | Some search, _ ->
                    window.SearchStatus.Text <-
                        [   sprintf "Search results for \"%s\"" search.Terms
                            (if search.CaseSensitive then "Case-sensitive" else "Not case-sensitive")
                            (if search.Regex then "Regular Expression" else "")
                            (if search.SubFolders then "Sub-Folders" else "")
                        ] |> List.filter String.isNotEmpty |> String.concat ", "
                    window.SearchPanel.Visible <- true
            )
            Bind.modelMulti(<@ model.IsSearchingSubFolders, model.Location, model.PathFormat @>)
                .toFunc(fun (sub, loc, fmt) -> setRelativePath (if sub then Some (loc, fmt) else None))
            Bind.modelMulti(<@ model.ShowHistoryType, model.Location, model.BackStack, model.ForwardStack,
                               model.UndoStack, model.RedoStack, model.History.Searches, model.SearchHistoryIndex,
                               model.StatusHistory, model.PathFormat @>)
                .toFunc(fun (historyType, location, back, forward, undo, redo, searches, searchIndex, statuses, pathFormat) ->
                    match historyType with
                    | Some historyType ->
                        let stackSize = 6
                        let maxStackSize = stackSize*2
                        let formatStack evt items =
                            let key = KeyBinding.getKeysString evt
                            items |> List.truncate maxStackSize |> List.mapi (fun i (name: string) ->
                                let repeat = if i > 0 then i + 1 |> string else ""
                                (repeat + key, name)
                            )
                        let header, prev, next, current =
                            match historyType with
                            | NavHistory ->
                                let format = List.map (fun (p: Path, _) -> p.Format pathFormat)
                                let current = Some (location.Format pathFormat)
                                ("Navigation History", back |> format |> formatStack Back, forward |> format |> formatStack Forward, current)
                            | UndoHistory ->
                                let format = List.map (fun (i: ItemAction) -> i.Description pathFormat)
                                ("Undo/Redo History", undo |> format |> formatStack Undo, redo |> format |> formatStack Redo, Some "---")
                            | SearchHistory ->
                                let format = List.map (fun s -> "", string s)
                                let prev =
                                    searches
                                    |> List.skip (searchIndex |> Option.map ((+) 1) |? 0)
                                    |> List.truncate maxStackSize
                                    |> format
                                let next =
                                    searches
                                    |> List.skip (max 0 ((searchIndex |? 0) - maxStackSize))
                                    |> List.truncate (min maxStackSize (searchIndex |? 0))
                                    |> format
                                    |> List.rev
                                let current = searchIndex |> Option.map (fun i -> string searches.[i])
                                ("Search History", prev, next, current)
                            | StatusHistory ->
                                let statusList =
                                    statuses |> List.map (function
                                        | Message m -> m
                                        | ErrorMessage m -> "Error: " + m
                                        | Busy _ -> ""
                                    ) |> List.map (fun m -> ("", m))
                                ("Status History", statusList, [], None)

                        let prev, next = trimStacks stackSize prev next
                        let isEmpty = prev.IsEmpty && next.IsEmpty
                        window.HistoryHeader.Text <- header
                        window.HistoryBack.ItemsSource <- prev |> List.rev
                        window.HistoryCurrentLabel.Text <- current |? ""
                        window.HistoryCurrent.Collapsed <- current.IsNone || isEmpty
                        window.HistoryForward.ItemsSource <- next
                        window.HistoryEmpty.Collapsed <- not isEmpty
                        window.HistoryPanel.Visible <- true
                    | None ->
                        window.HistoryPanel.Visible <- false
                )

            // update UI for status
            Bind.modelMulti(<@ model.Status, model.KeyCombo, model.RepeatCommand @>)
                .toFunc(fun (status, keyCombo, repeatCommand) ->
                let statusText, errorText =
                    if keyCombo |> Seq.isNotEmpty then
                        let msg =
                            keyCombo
                            |> Seq.map KeyBinding.keyDescription
                            |> String.concat ""
                            |> sprintf "Pressed %s, waiting for another key..."
                        (msg, "")
                    elif repeatCommand.IsSome then
                        let plural = if repeatCommand.Value <> 1 then "s" else ""
                        (sprintf "Repeat command %i time%s..." repeatCommand.Value plural, "")
                    else
                        match status with
                        | Some (Message msg)
                        | Some (Busy msg) -> (msg, "")
                        | Some (ErrorMessage msg) -> ("", msg)
                        | None -> ("", "")
                window.StatusText.Text <- statusText
                window.StatusText.Collapsed <- statusText |> String.isEmpty
                window.ErrorText.Text <- errorText
                window.ErrorText.Collapsed <- errorText |> String.isEmpty
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

    let getFileDropPaths (data: IDataObject) =
        data.GetData(DataFormats.FileDrop) :?> string array
        |> Option.ofObj
        |? [||]
        |> Seq.choose Path.Parse
        |> Seq.toList

    let events (config: ConfigFile) (history: HistoryFile) (subDirResults: IObservable<_>) (window: MainWindow) = [
        window.PathBox.PreviewKeyDown |> Obs.filter isNotModifier |> Obs.choose (fun evt ->
            let keyPress = KeyPress (evt.Chord, evt.Handler)
            let ignoreMods = [ ModifierKeys.None; ModifierKeys.Shift ]
            let ignoreCtrlKeys = [ Key.A; Key.Z; Key.X; Key.C; Key.V ]
            let focusGrid () = window.ItemGrid.Focus() |> ignore
            let selectedPath = window.PathSuggestions.SelectedItem |> unbox |> Option.ofString
            let path = selectedPath |? window.PathBox.Text
            match evt.Chord with
            | (ModifierKeys.None, Key.Enter) -> Some (OpenPath (path, evt.HandlerWithEffect focusGrid))
            | (ModifierKeys.None, Key.Escape) -> focusGrid(); Some ResetLocationInput
            | (ModifierKeys.None, Key.Delete) ->
                selectedPath
                |> Option.bind Path.Parse
                |> Option.map (tee (fun _ -> evt.Handled <- true) >> DeletePathSuggestion)
            | (ModifierKeys.Control, key) when ignoreCtrlKeys |> List.contains key -> None
            | (modifier, _) when not (ignoreMods |> List.contains modifier) -> Some keyPress
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
                      |> Obs.onCurrent
                      |> Obs.map (List.concat >> SubDirectoryResults)
        window.InputBox.LostFocus |> Obs.mapTo CancelInput

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
        window.ItemGrid.DragOver |> Obs.map (fun (e: DragEventArgs) ->
            let paths = getFileDropPaths e.Data
            e.Handled <- true
            UpdateDropInAction (paths, DragEvent e)
        )
        window.ItemGrid.MouseMove |> Obs.choose (fun e ->
            if e.LeftButton = MouseButtonState.Pressed then
                let item = window.ItemGrid.SelectedItem :?> Item
                let dropData = DataObject(DataFormats.FileDrop, [|item.Path.Format Windows|])
                DragDrop.DoDragDrop(window.ItemGrid, dropData, DragDropEffects.Move ||| DragDropEffects.Copy ||| DragDropEffects.Link)
                |> DragDropEffects.toActions
                |> List.tryHead
                |> Option.map DropOut
            else None
        )
        window.ItemGrid.Drop |> Obs.choose (fun e ->
            let paths = getFileDropPaths e.Data
            if paths |> List.isEmpty then
                None
            else
                e.Handled <- true
                Some (DropIn (paths, DragEvent e))
        )
        config.FileChanged |> Obs.onCurrent |> Obs.map ConfigFileChanged
        history.FileChanged |> Obs.onCurrent |> Obs.map HistoryFileChanged
    ]

module MainStatus =
    // navigation
    let find prefix repeatCount =
        if repeatCount = 1 then
            Message <| sprintf "Find item starting with: %s" prefix
        else
            Message <| sprintf "Find every %i items starting with: %s" repeatCount prefix
    let noBookmark char = Message <| sprintf "Bookmark \"%c\" not set" char
    let setBookmark char path = Message <| sprintf "Set bookmark \"%c\" to %s" char path
    let deletedBookmark char path = Message <| sprintf "Deleted bookmark \"%c\" that was set to %s" char path
    let noSavedSearch char = Message <| sprintf "Saved Search \"%c\" not set" char
    let setSavedSearch char (search: Search) = Message <| sprintf "Set saved search \"%c\" to \"%s\"" char (string search)
    let deletedSavedSearch char  (search: Search) = Message <| sprintf "Deleted saved search \"%c\" that was set to \"%s\"" char (string search)

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
        | PutItems (Move, item, newPath) -> Some <| sprintf "Moving %s to \"%s\"..." item.Description (newPath.Format pathFormat)
        | PutItems (Copy, item, newPath) -> Some <| sprintf "Copying %s to \"%s\"..." item.Description (newPath.Format pathFormat)
        | DeletedItem (item, false) -> Some <| sprintf "Recycling %s..." item.Description
        | DeletedItem (item, true) -> Some <| sprintf "Deleting %s..." item.Description
        | _ -> None
    let runningAction action pathFormat =
        runningActionMessage action pathFormat |> Option.map Busy
    let preparingPut (action: PutAction) name = Busy <| sprintf "Preparing to %O %s..." action name
    let checkingIsRecyclable = Busy "Calculating size..."
    let private actionCompleteMessage action pathFormat =
        match action with
        | CreatedItem item -> sprintf "Created %s" item.Description
        | RenamedItem (item, newName) -> sprintf "Renamed %s to \"%s\"" item.Description newName
        | PutItems (Move, item, newPath) -> sprintf "Moved %s to \"%s\"" item.Description (newPath.Format pathFormat)
        | PutItems (Copy, item, newPath) -> sprintf "Copied %s to \"%s\"" item.Description (newPath.Format pathFormat)
        | PutItems (Shortcut, item, _) -> sprintf "Created shortcut to %s \"%s\""
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
    let undoAction action pathFormat repeatCount =
        let prefix =
            if repeatCount = 1 then
                "Action undone: "
            else
                sprintf "%i actions undone. Last: " repeatCount
        Message (prefix + actionCompleteMessage action pathFormat)

    let redoingAction action pathFormat =
        runningActionMessage action pathFormat
            |> Option.map (fun m -> Busy <| sprintf "Redoing action: %s" m)
    let redoAction action pathFormat repeatCount =
        let prefix =
            if repeatCount = 1 then
                "Action redone: "
            else
                sprintf "%i actions redone. Last: " repeatCount
        Message (prefix + actionCompleteMessage action pathFormat)


type MainError =
    | ActionError of actionName: string * exn
    | ItemActionError of ItemAction * PathFormat * exn
    | PutError of PutAction * msg: string * errors: int * total: int
    | InvalidPath of string
    | ShortcutTargetMissing of string
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
            (ActionError (action.Description pathFormat, e)).Message
        | PutError (action, msg, errors, total) ->
            let action = action |> string |> String.toLower
            sprintf "Could not %s %i of %i items: %s" action errors total msg
        | InvalidPath path -> "Path format is invalid: " + path
        | ShortcutTargetMissing path -> "Shortcut target does not exist: " + path
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
            this.WithStatus (ErrorMessage e.Message)
