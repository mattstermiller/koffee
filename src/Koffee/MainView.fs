namespace Koffee

open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.ComponentModel
open System.Reactive.Subjects
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp
open UIHelpers
open KoffeeUI

module Obs = Observable

module MainView =
    type HistoryPanelRow = {
        Header: string
        Content: string
        IsListTraversable: bool
        IsCurrent: bool
    }
    with
        static member createFlat (header, content) = {
            Header = header
            Content = content
            IsListTraversable = false
            IsCurrent = false
        }

        static member createTraversable isCurrent (header, content) = {
            Header = header
            Content = content
            IsListTraversable = true
            IsCurrent = isCurrent
        }

    let private itemCountAndSize name items =
        let sizeStr =
            items
            |> List.choose (fun n -> if n.Type = File then n.Size else None)
            |> Option.ofCond (not << List.isEmpty)
            |> Option.map (List.sum >> Format.fileSize >> sprintf ", %s")
            |> Option.toString
        sprintf "%i %s%s" items.Length name sizeStr

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
                if paths.IsVisible then
                    selectedPath |> Option.iter window.PathBox.set_Text
                window.PathBox.Select(window.PathBox.Text.Length, 0)
                e.Handled <- true
            | _ -> ()
        )
        window.PathBox.LostFocus.Add (fun _ -> window.PathSuggestions.IsHidden <- true)

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
            | Name -> 1
            | Modified -> 3
            | Size -> 4
        let setRelativePath relInfo =
            relativePathFormat <-
                match relInfo with
                | Some (path, fmt) -> (fun p -> p.FormatRelativeFolder fmt path)
                | None -> string
            window.ItemGrid.Columns.[2].IsCollapsed <- relInfo.IsNone

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
                window.Progress.IsCollapsed <- incr.IsNone
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
        [
            Bind.view(<@ window.PathBox.Text @>).toModel(<@ model.LocationInput @>, OnChange)
            Bind.modelMulti(<@ model.PathSuggestions, model.PathFormat @>).toFunc(fun (paths, pathFormat) ->
                match paths with
                | Ok paths ->
                    let paths = paths |> Seq.map (fun p -> p.Format pathFormat) |> Seq.toArray
                    window.PathSuggestions.ItemsSource <- paths
                    window.PathSuggestions.SelectedIndex <- if paths.Length = 1 then 0 else -1
                    window.PathSuggestions.IsEnabled <- true
                    window.PathSuggestions.IsHidden <- not window.PathBox.IsFocused || (paths |> Array.isEmpty)
                | Error error ->
                    window.PathSuggestions.ItemsSource <- ["Error: " + error]
                    window.PathSuggestions.IsEnabled <- false
                    window.PathSuggestions.IsHidden <- not window.PathBox.IsFocused
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
            Bind.model(<@ model.SelectedItems @>).toFunc(fun selected ->
                window.ItemGrid.Tag <- selected
                if not selected.IsEmpty then
                    window.SelectedStatus.Text <- itemCountAndSize "selected" selected
                window.SelectedStatusPanel.IsCollapsed <- selected.IsEmpty
            )
            Bind.model(<@ model.Items @>).toFunc(fun items ->
                let name = (if items.Length = 1 then "item" else "items")
                window.DirectoryStatus.Text <- itemCountAndSize name items
            )
            Bind.view(<@ window.ItemGrid.SelectedIndex @>).toModelOneWay(<@ model.Cursor @>)

            // display path
            Bind.model(<@ model.TitleLocation @>).toFunc(fun titleLoc ->
                window.Title <- sprintf "%s  |  Koffee v%s" titleLoc versionStr
            )

            // display yank register
            let getToggledStarGridLength toggle =
                if toggle then GridLength(1, GridUnitType.Star) else GridLength(0)
            Bind.model(<@ model.Config.YankRegister @>).toFunc(fun register ->
                let text =
                    register |> Option.bind (fun (putType, itemRefs) ->
                        match itemRefs with
                        | itemRef :: rest ->
                            let restDescr = if rest.IsEmpty then "" else sprintf " and %i more" rest.Length
                            Some (sprintf "%A: %s %s%s" putType itemRef.Type.Symbol itemRef.Path.Name restDescr)
                        | [] -> None
                    )
                window.YankRegisterText.Text <- text |? ""
                window.YankRegisterColumn.Width <- getToggledStarGridLength text.IsSome
            )

            // display next undo and redo actions
            let getUndoRedoDisplay prefix (actionStack: ItemAction list, pathFormat) =
                actionStack
                |> List.tryHead
                |> Option.map (fun action -> prefix + action.ShortDescription pathFormat)
            Bind.modelMulti(<@ model.UndoStack, model.PathFormat @>).toFunc(getUndoRedoDisplay "⭮ Undo: " >> fun text ->
                window.UndoActionText.Text <- text |? ""
                window.UndoActionColumn.Width <- getToggledStarGridLength text.IsSome
            )
            Bind.modelMulti(<@ model.RedoStack, model.PathFormat @>).toFunc(getUndoRedoDisplay "🡪 Redo: " >> fun text ->
                window.RedoActionText.Text <- text |? ""
                window.RedoActionColumn.Width <- getToggledStarGridLength text.IsSome
            )

            // update UI for status
            Bind.modelMulti(<@ model.Status, model.KeyCombo, model.RepeatCommand, model.PathFormat @>)
                .toFunc(fun (status, keyCombo, repeatCommand, pathFormat) ->
                let statusText, errorText =
                    if keyCombo |> Seq.isNotEmpty then
                        let msg =
                            keyCombo
                            |> Seq.map KeyBinding.keyDescription
                            |> String.concat ""
                            |> sprintf "Pressed %s, waiting for another key..."
                        (msg, "")
                    elif repeatCommand.IsSome then
                        let msg =
                            repeatCommand.Value
                            |> Format.count "time"
                            |> sprintf "Repeat command %s..."
                        (msg, "")
                    else
                        match status with
                        | Some (MainStatus.Message msg) -> (msg.Message pathFormat, "")
                        | Some (MainStatus.Busy msg) -> (msg.Message pathFormat, "")
                        | Some (MainStatus.Error error) -> ("", error.Message pathFormat)
                        | None -> ("", "")
                window.StatusText.Text <- statusText
                window.ErrorText.Text <- errorText
                let isBusy =
                    match status with
                    | Some (MainStatus.Busy _) -> true
                    | _ -> false
                let wasBusy = not window.ItemGrid.IsEnabled
                window.PathBox.IsEnabled <- not isBusy
                window.ItemGrid.IsEnabled <- not isBusy
                window.Cursor <- if isBusy then Cursors.Wait else Cursors.Arrow
                if wasBusy && not isBusy then
                    window.ItemGrid.Focus() |> ignore
            )

            // update UI for input mode
            Bind.view(<@ window.InputBox.Text @>).toModel(<@ model.InputText @>, OnChange)
            Bind.view(<@ window.SearchCaseSensitive.IsChecked @>).toModel(<@ model.SearchInput.CaseSensitive @>, ((=) (Nullable true)), Nullable)
            Bind.view(<@ window.SearchRegex.IsChecked @>).toModel(<@ model.SearchInput.Regex @>, ((=) (Nullable true)), Nullable)
            Bind.view(<@ window.SearchSubFolders.IsChecked @>).toModel(<@ model.SearchInput.SubFolders @>, ((=) (Nullable true)), Nullable)
            Bind.modelMulti(<@ model.InputMode, model.InputTextSelection, model.PathFormat @>)
                .toFunc(fun (inputMode, (selectStart, selectLen), pathFormat) ->
                    match inputMode with
                    | Some inputMode ->
                        window.SearchOptions.IsCollapsed <- inputMode <> Input Search
                        window.InputText.Text <- inputMode.GetPrompt pathFormat
                        if window.InputPanel.IsCollapsed then
                            window.InputPanel.IsCollapsed <- false
                            window.InputBox.Select(selectStart, selectLen)
                            window.InputBox.Focus() |> ignore
                    | None ->
                        if not window.InputPanel.IsCollapsed then
                            window.InputPanel.IsCollapsed <- true
                            window.ItemGrid.Focus() |> ignore
                )
            Bind.model(<@ model.InputTextSelection @>).toFunc(fun (selectStart, selectLen) ->
                window.InputBox.Select(selectStart, selectLen)
            )
            Bind.model(<@ model.InputError @>).toFunc(function
                | Some error ->
                    window.InputError.Text <- error.Message
                    window.InputErrorPanel.IsCollapsed <- false
                | None ->
                    window.InputErrorPanel.IsCollapsed <- true
            )
            Bind.modelMulti(<@ model.SearchCurrent, model.InputMode @>).toFunc(function
                | None, _
                | Some _, Some (Input Search) ->
                    window.SearchPanel.IsCollapsed <- true
                | Some search, _ ->
                    window.SearchStatus.Text <-
                        [   sprintf "Search results for \"%s\"" search.Terms
                            (if search.CaseSensitive then "Case-sensitive" else "Not case-sensitive")
                            (if search.Regex then "Regular Expression" else "")
                            (if search.SubFolders then "Sub-Folders" else "")
                        ] |> List.filter String.isNotEmpty |> String.concat ", "
                    window.SearchPanel.IsCollapsed <- false
            )
            Bind.modelMulti(<@ model.IsSearchingSubFolders, model.Location, model.PathFormat @>)
                .toFunc(fun (sub, loc, fmt) -> setRelativePath (if sub then Some (loc, fmt) else None))
            Bind.modelMulti(<@ model.HistoryDisplay, model.InputMode, model.Location, model.BackStack, model.ForwardStack,
                               model.UndoStack, model.RedoStack, model.History.Searches, model.SearchHistoryIndex,
                               model.StatusHistory, model.Config.Bookmarks, model.Config.SavedSearches, model.PathFormat @>)
                .toFunc(fun (historyType, inputMode, location, back, forward, undo, redo, searches, searchIndex, statuses,
                             bookmarks, savedSearches, pathFormat) ->
                    let showHistoryType =
                        inputMode
                        |> Option.map (fun input -> input.HistoryDisplay)
                        |> Option.defaultValue historyType
                    match showHistoryType with
                    | Some historyType ->
                        let maxStackSize = 6
                        let maxListSize = maxStackSize*2
                        let flatList (history: (string*string) list) =
                            history |> List.truncate maxListSize |> List.rev |> List.map HistoryPanelRow.createFlat
                        let traversableList (prev: (string*string) list) (next: (string*string) list) (current: string option) =
                            let prev, next = trimStacks maxStackSize prev next
                            seq {
                                yield! prev |> Seq.rev |> Seq.map (HistoryPanelRow.createTraversable false)
                                yield!
                                    current
                                    |> Option.map (fun content ->
                                        ("(current)", content) |> HistoryPanelRow.createTraversable true
                                    )
                                    |> Option.toList
                                yield! next |> Seq.map (HistoryPanelRow.createTraversable false)
                            }
                            |> Seq.toList
                        let formatStack evt items =
                            let key = KeyBinding.getKeysString evt
                            items |> List.truncate maxListSize |> List.mapi (fun i (name: string) ->
                                let repeat = if i > 0 then i + 1 |> string else ""
                                (repeat + key, name)
                            )
                        let header, rows =
                            match historyType with
                            | NavHistory ->
                                let format = List.map (fun (p: Path, _) -> p.Format pathFormat)
                                let rows =
                                    traversableList
                                        (back |> format |> formatStack (Navigation Back))
                                        (forward |> format |> formatStack (Navigation Forward))
                                        (Some (location.Format pathFormat))
                                ("Navigation History", rows)
                            | UndoHistory ->
                                let format = List.map (fun (i: ItemAction) -> i.Description pathFormat)
                                let rows =
                                    traversableList
                                        (undo |> format |> formatStack (ItemAction Undo))
                                        (redo |> format |> formatStack (ItemAction Redo))
                                        (if undo.IsEmpty && redo.IsEmpty then None else Some "---")
                                ("Undo/Redo History", rows)
                            | SearchHistory ->
                                let format = List.map (fun s -> "", string s)
                                let prev =
                                    searches
                                    |> List.skip (searchIndex |> Option.map ((+) 1) |? 0)
                                    |> List.truncate maxListSize
                                    |> format
                                let next =
                                    searches
                                    |> List.skip (max 0 ((searchIndex |? 0) - maxListSize))
                                    |> List.truncate (min maxListSize (searchIndex |? 0))
                                    |> format
                                    |> List.rev
                                let current = searchIndex |> Option.map (fun i -> string searches.[i])
                                ("Search History", traversableList prev next current)
                            | StatusHistory ->
                                let rows =
                                    statuses
                                    |> List.map (function
                                        | MainStatus.Message m -> m.Message pathFormat
                                        | MainStatus.Error error -> "Error: " + error.Message pathFormat
                                        | MainStatus.Busy _ -> ""
                                    )
                                    |> List.mapi (fun i msg -> (i+1 |> string, msg))
                                    |> flatList
                                ("Status History", rows)
                            | Bookmarks ->
                                let rows =
                                    bookmarks
                                    |> List.map (fun (char, path) -> (string char, path.Format pathFormat))
                                    |> List.map HistoryPanelRow.createFlat
                                ("Bookmarks", rows)
                            | SavedSearches ->
                                let rows =
                                    savedSearches
                                    |> List.map (fun (char, search) -> string char, string search)
                                    |> List.map HistoryPanelRow.createFlat
                                ("Saved Searches", rows)

                        window.HistoryHeader.Text <- header
                        window.HistoryItems.ItemsSource <-
                            rows |> Seq.ifEmpty [HistoryPanelRow.createFlat ("", "Nothing here")]
                        window.HistoryPanel.IsCollapsed <- false
                    | None ->
                        window.HistoryPanel.IsCollapsed <- true
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
        let tryGetData format =
            try
                data.GetData(format, true)
            with _ ->
                null
        tryGetData DataFormats.FileDrop :?> string array |> Option.ofObj
        |> Option.orElseWith (fun () ->
            tryGetData DataFormats.Text :?> string |> Option.ofObj |> Option.map (String.split '\n')
        )
        |? [||]
        |> Array.choose Path.Parse
        |> Array.toList

    let events (config: ConfigFile) (history: HistoryFile) (subDirResults: IObservable<_>) (window: MainWindow) =
        [
            window.KeyDown
                |> Obs.filter (fun evt -> evt.Chord = (ModifierKeys.None, Key.Escape))
                |> Obs.map (fun evt -> KeyPress (evt.Chord, evt.Handler))
            window.PathBox.PreviewKeyDown |> Obs.filter isNotModifier |> Obs.choose (fun evt ->
                let keyPress = KeyPress (evt.Chord, evt.Handler)
                let ignoreMods = [ ModifierKeys.None; ModifierKeys.Shift ]
                let ignoreCtrlKeys = [ Key.A; Key.Z; Key.X; Key.C; Key.V ]
                let focusGrid () = window.ItemGrid.Focus() |> ignore
                let selectedPath = window.PathSuggestions.SelectedItem |> unbox |> Option.ofString
                let path = selectedPath |? window.PathBox.Text
                match evt.Chord with
                | (ModifierKeys.None, Key.Enter) -> Some (LocationInputSubmit (path, evt.HandlerWithEffect focusGrid))
                | (ModifierKeys.None, Key.Escape) -> focusGrid(); Some LocationInputCancel
                | (ModifierKeys.None, Key.Delete) ->
                    selectedPath
                    |> Option.bind HistoryPath.Parse
                    |> Option.map (tee (fun _ -> evt.Handled <- true) >> DeletePathSuggestion)
                | (ModifierKeys.Control, key) when ignoreCtrlKeys |> List.contains key -> None
                | (modifier, _) when not (ignoreMods |> List.contains modifier) -> Some keyPress
                | (_, key) when key >= Key.F1 && key <= Key.F12 -> Some keyPress
                | _ -> None
            )
            window.PathBox.TextChanged
                |> Obs.filter (fun _ -> window.PathBox.IsFocused)
                |> Obs.mapTo LocationInputChanged
            window.SettingsButton.Click |> Obs.mapTo SettingsButtonClick

            window.ItemGrid.PreviewKeyDown
                |> Obs.filter isNotModifier
                |> Obs.map (fun evt -> KeyPress (evt.Chord, evt.Handler))
            window.ItemGrid.PreviewKeyDown |> Obs.choose (fun evt ->
                if evt.Chord = (ModifierKeys.Control, Key.C) then
                    evt.Handled <- true // prevent Ctrl+C crash due to bug in WPF datagrid
                None
            )
            window.ItemGrid.MouseDoubleClick |> Obs.mapTo ItemDoubleClick
            window.ItemGrid.SizeChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.choose (fun _ ->
                let grid = window.ItemGrid
                if grid.HasItems then
                    let visibleIndex = grid.SelectedIndex |> max 0
                    grid.ItemContainerGenerator.ContainerFromIndex(visibleIndex) :?> DataGridRow |> Option.ofObj
                    |> Option.map (fun visibleRow  ->
                        let viewHeight = grid.ActualHeight - grid.ActualColumnHeaderHeight
                        let pageSize = viewHeight / visibleRow.ActualHeight |> int
                        Background (PageSizeChanged pageSize)
                    )
                else None
            )

            window.InputBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> InputSubmit)
            window.InputBox.PreviewKeyDown |> Obs.choose (fun keyEvt ->
                match keyEvt.Key with
                | Key.Up -> Some InputBack
                | Key.Down -> Some InputForward
                | Key.Delete -> Some (InputDelete (Keyboard.Modifiers = ModifierKeys.Shift, keyEvt.Handler))
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
            subDirResults
                |> Obs.buffer 0.3
                |> Obs.onCurrent
                |> Obs.map (List.concat >> SubDirectoryResults)
            window.InputBox.LostFocus |> Obs.mapTo InputCancel

            window.Activated |> Obs.filter (fun _ -> window.IsLoaded) |> Obs.mapTo WindowActivated
            window.LocationChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.choose (fun _ ->
                if window.Left > -window.Width && window.Top > -window.Height then
                    Some (Background (WindowLocationChanged (int window.Left, int window.Top)))
                else None
            )
            window.SizeChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.map (fun _ ->
                Background (WindowSizeChanged (int window.Width, int window.Height))
            )
            window.StateChanged |> Obs.choose (fun _ ->
                if window.WindowState <> WindowState.Minimized then
                    Some (Background (WindowMaximizedChanged (window.WindowState = WindowState.Maximized)))
                else None
            )
            window.ItemGrid.DragOver |> Obs.map (fun (e: DragEventArgs) ->
                let paths = getFileDropPaths e.Data
                e.Handled <- true
                UpdateDropInPutType (paths, DragInEvent e)
            )
            window.ItemGrid.MouseMove |> Obs.choose (fun e ->
                if e.LeftButton = MouseButtonState.Pressed then
                    Some (DropOut (DragOutEvent window.ItemGrid))
                else
                    None
            )
            window.ItemGrid.Drop |> Obs.choose (fun e ->
                let paths = getFileDropPaths e.Data
                if paths |> List.isEmpty then
                    None
                else
                    e.Handled <- true
                    Some (DropIn (paths, DragInEvent e))
            )
            config.FileChanged |> Obs.onCurrent |> Obs.map (ConfigFileChanged >> Background)
            history.FileChanged |> Obs.onCurrent |> Obs.map (HistoryFileChanged >> Background)
        ]
