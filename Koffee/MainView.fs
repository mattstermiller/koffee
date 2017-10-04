namespace Koffee

open System.ComponentModel
open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Media
open FSharp.Desktop.UI
open ModelExtensions
open UIHelpers
open Utility
open Reflection
open ConfigExt

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow, keyBindings: (KeyCombo * MainEvents) list, config: Config) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    let mutable currBindings = keyBindings

    let onKeyFunc key resultFunc (keyEvent : IEvent<KeyEventHandler, KeyEventArgs>) =
        keyEvent |> Observable.choose (fun evt ->
            if evt.Key = key then
                evt.Handled <- true
                Some <| resultFunc()
            else
                None)

    override this.SetBindings (model: MainModel) =
        // setup grid
        window.NodeGrid.AddColumn("Name", widthWeight = 3.0)
        window.NodeGrid.AddColumn("Type", converter = ValueConverters.UnionText())
        window.NodeGrid.AddColumn("Modified", converter = ValueConverters.OptionValue(), format = FormatString.dateTime)
        window.NodeGrid.AddColumn("SizeFormatted", "Size", alignRight = true)
        window.NodeGrid.Columns |> Seq.iter (fun c -> c.CanUserSort <- false)

        // simple bindings
        Binding.OfExpression
            <@
                window.NodeGrid.ItemsSource <- model.Nodes
                window.NodeGrid.SelectedIndex <- model.Cursor
                window.CommandBox.Text <- model.CommandText |> BindingOptions.UpdateSourceOnChange
            @>

        // display path
        let displayPath _ = window.Dispatcher.Invoke(fun () ->
            window.PathBox.Text <- model.PathFormatted
            let displayPath = if model.ShowFullPathInTitle then model.PathFormatted else model.Path.Name
            window.Title <-
                displayPath
                |> Str.ifEmpty model.PathFormatted
                |> sprintf "%s  |  Koffee")
        bindPropertyToFunc <@ model.Path @> displayPath
        model.OnPropertyChanged <@ model.PathFormat @> displayPath
        model.OnPropertyChanged <@ model.ShowFullPathInTitle @> displayPath

        // register display and save
        bindPropertyToFunc <@ model.YankRegister @> (fun register ->
            config.YankRegister <- register |> Option.map (fun (node, action) -> node.Path, action)
            config.Save()
            let text = MainView.RegisterStatus register
            window.Dispatcher.Invoke (fun () ->
                window.RegisterLabel.Content <- text
                window.RegisterLabel.Visible <- text <> ""))

        // update command text selection
        bindPropertyToFunc <@ model.CommandTextSelection @> (fun (start, len) ->
            window.CommandBox.Select(start, len))

        // update UI for the command input mode
        bindPropertyToFunc <@ model.CommandInputMode @> (fun mode ->
            this.CommandInputModeChanged mode model.SelectedNode
            if mode.IsNone then model.CommandTextSelection <- (999, 0))

        // update UI for status
        bindPropertyToFunc <@ model.Status @> this.UpdateStatus

        // bind Tab key to switch focus
        window.PathBox.PreviewKeyDown.Add <| onKey Key.Tab window.NodeGrid.Focus
        window.NodeGrid.PreviewKeyDown.Add <| onKey Key.Tab (fun () ->
            window.PathBox.SelectAll()
            window.PathBox.Focus())

        // on selection change, keep selected node in view, make sure node list is focused
        window.NodeGrid.SelectionChanged.Add (fun _ ->
            this.KeepSelectedInView()
            window.NodeGrid.Focus() |> ignore)

        // on resize, keep selected node in view, update the page size
        window.NodeGrid.SizeChanged.Add (fun _ ->
            this.KeepSelectedInView()
            this.ItemsPerPage |> Option.iter (fun i -> model.PageSize <- i))

        // escape and lost focus resets the input mode
        window.PreviewKeyDown.Add <| onKey Key.Escape (fun () ->
            model.Status <- None
            model.CommandInputMode <- None
            window.NodeGrid.Focus() |> ignore)
        window.CommandBox.LostFocus.Add (fun _ -> model.CommandInputMode <- None)

        // pressing delete when viewing bookmarks allows delete
        window.CommandBox.PreviewKeyDown.Add (fun evt ->
            if evt.Key = Key.Delete then
                match model.CommandInputMode with
                | Some GoToBookmark | Some SetBookmark ->
                    evt.Handled <- true
                    model.CommandInputMode <- Some DeleteBookmark
                | _ -> ())

        // make sure selected item gets set to the cursor
        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

        // load window settings
        window.Left <- float config.Window.Left
        window.Top <- float config.Window.Top
        window.Width <- float config.Window.Width
        window.Height <- float config.Window.Height
        if config.Window.IsMaximized then
            window.WindowState <- WindowState.Maximized

        // setup saving window settings
        window.StateChanged.Add (fun _ -> 
            if window.WindowState <> WindowState.Minimized then
                config.Window.IsMaximized <- window.WindowState = WindowState.Maximized
                config.Save())
        window.LocationChanged.Add (fun _ ->
            if window.Left >= 0.0 && window.Top >= 0.0 then
                config.Window.Left <- int window.Left
                config.Window.Top <- int window.Top
                config.Save())
        window.SizeChanged.Add (fun e -> 
            config.Window.Width <- int window.Width
            config.Window.Height <- int window.Height
            config.Save())

        window.Closed.Add (fun _ ->
            config.PreviousPath <- model.Path.Format Windows
            config.Save())

    override this.EventStreams = [
        window.Activated |> Observable.choose this.Activated
        window.PathBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> OpenPath window.PathBox.Text)
        window.PathBox.PreviewKeyDown |> Observable.choose (fun keyEvt ->
            match this.TriggerKeyBindings keyEvt with
            | Some Exit -> Some Exit
            | _ -> keyEvt.Handled <- false; None)
        window.SettingsButton.Click |> Observable.mapTo OpenSettings
        window.NodeGrid.PreviewKeyDown |> Observable.choose this.TriggerKeyBindings
        window.CommandBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> ExecuteCommand)
        window.CommandBox.PreviewTextInput |> Observable.choose this.CommandTextInput
    ]

    member this.Activated _ =
        if config.Window.RefreshOnActivate then
            Some Refresh
        else None

    member this.CommandTextInput keyEvt =
        match keyEvt.Text.ToCharArray() with
        | [| c |] -> Some (CommandCharTyped c)
        | _ -> None

    member this.TriggerKeyBindings evt =
        let modifierKeys = [
            Key.LeftShift; Key.RightShift; Key.LeftCtrl; Key.RightCtrl;
            Key.LeftAlt; Key.RightAlt; Key.LWin; Key.RWin; Key.System
        ]
        let chord = (Keyboard.Modifiers, evt.Key)

        if Seq.contains evt.Key modifierKeys then
            None
        else if chord = (ModifierKeys.None, Key.Escape) then
            evt.Handled <- true
            currBindings <- keyBindings
            None
        else
            match KeyBinding.GetMatch currBindings chord with
            // completed key combo
            | [ ([], Exit) ] ->
                window.Close()
                None
            | [ ([], newEvent) ] ->
                evt.Handled <- true
                currBindings <- keyBindings
                Some newEvent
            // no match
            | [] ->
                currBindings <- keyBindings
                None
            // partial match to one or more key combos
            | matchedBindings ->
                evt.Handled <- true
                currBindings <- matchedBindings
                None

    member this.UpdateStatus status =
        window.StatusLabel.Content <- 
            match status with
            | Some (Message msg) | Some (ErrorMessage msg) | Some (Busy msg) -> msg
            | None -> ""
        window.StatusLabel.Foreground <-
            match status with
            | Some (ErrorMessage _) -> Brushes.Red
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

    member this.CommandInputModeChanged mode node =
        match mode with
        | Some inputMode ->
            let isBookmark = Seq.contains inputMode [GoToBookmark; SetBookmark; DeleteBookmark]
            if isBookmark then
                let bookmarks =
                    match config.GetBookmarks() with
                    | bm when bm |> Seq.isEmpty -> [(' ', "No bookmarks set")] |> dict
                    | bm -> bm
                window.Bookmarks.ItemsSource <- bookmarks
            window.BookmarkPanel.Visible <- isBookmark
            this.ShowCommandBar (inputMode |> this.GetPrompt node)
        | None -> this.HideCommandBar ()

    member this.GetPrompt (node: Node) = function
        | Confirm (Overwrite (_, src, dest)) ->
            match dest.Type with
            | Folder -> sprintf "Folder \"%s\" already exists. Move anyway and merge files y/n ?" dest.Name
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
        | Confirm Delete -> sprintf "Permanently delete %s y/n ?" node.Description
        | inputType -> inputType |> GetUnionCaseName |> Str.readableIdentifier |> sprintf "%s:"

    member private this.ShowCommandBar label =
        window.CommandLabel.Content <- label
        window.CommandPanel.Visible <- true
        window.CommandBox.Focus() |> ignore

    member private this.HideCommandBar () =
        window.CommandPanel.Visible <- false
        window.BookmarkPanel.Visible <- false
        window.NodeGrid.Focus() |> ignore

    member this.KeepSelectedInView () =
        if window.NodeGrid.SelectedItem <> null then
            window.NodeGrid.ScrollIntoView(window.NodeGrid.SelectedItem)

    member this.ItemsPerPage =
        if window.NodeGrid.HasItems then
            let index = window.NodeGrid.SelectedIndex |> max 0
            let row = window.NodeGrid.ItemContainerGenerator.ContainerFromIndex(index) :?> DataGridRow
            window.NodeGrid.ActualHeight / row.ActualHeight |> int |> Some
        else
            None

    static member RegisterStatus = function
        | Some (node, action) -> sprintf "%A %A: %s" action node.Type node.Name
        | None -> ""

module MainStatus =
    // navigation
    let find char = Message <| sprintf "Find %O" char
    let search matches searchStr = Message <| sprintf "Search \"%s\" found %i matches" searchStr matches
    let isSearchStatus searchStr status =
        match status with
        | Some (Message s) -> s.StartsWith(sprintf "Search \"%s\"" searchStr)
        | _ -> false
    let noBookmark char = Message <| sprintf "Bookmark \"%c\" not set" char
    let setBookmark char path = Message <| sprintf "Set bookmark \"%c\" to %s" char path
    let deletedBookmark char path = Message <| sprintf "Deleted bookmark \"%c\" that was set to %s" char path

    // actions
    let invalidPath path = ErrorMessage <| sprintf "Path format is invalid: %s" path
    let sort field desc = Message <| sprintf "Sort by %A %s" field (if desc then "descending" else "ascending")
    let toggleHidden showing = Message <| sprintf "%s hidden files" (if showing then "Showing" else "Hiding")
    let openFile name = Message <| sprintf "Opened File: %s" name
    let couldNotOpenFile name error = ErrorMessage <| sprintf "Could not open %s: %s" name error
    let openExplorer = Message "Opened Windows Explorer"
    let openCommandLine path = Message <| sprintf "Opened Commandline at: %s" path
    let openTextEditor name = Message <| sprintf "Opened text editor for: %s" name
    let couldNotOpenTextEditor error = ErrorMessage <| sprintf "Could not open text editor: %s" error

    let private runningActionMessage action pathFormat =
        match action with
        | MovedItem (node, newPath) -> Some <| sprintf "Moving %s to \"%s\"..." node.Description (newPath.Format pathFormat)
        | CopiedItem (node, newPath) -> Some <| sprintf "Copying %s to \"%s\"..." node.Description (newPath.Format pathFormat)
        | DeletedItem (node, false) -> Some <| sprintf "Recycling %s..." node.Description
        | DeletedItem (node, true) -> Some <| sprintf "Deleting %s..." node.Description
        | _ -> None
    let checkingIsRecyclable = Message <| "Calculating size..."
    let runningAction action pathFormat =
        runningActionMessage action pathFormat |> Option.map (fun m -> Busy m)
    let private actionCompleteMessage action pathFormat =
        match action with
        | CreatedItem node -> sprintf "Created %s" node.Description
        | RenamedItem (node, newName) -> sprintf "Renamed %s to \"%s\"" node.Description newName
        | MovedItem (node, newPath) -> sprintf "Moved %s to \"%s\"" node.Description (newPath.Format pathFormat)
        | CopiedItem (node, newPath) -> sprintf "Copied %s to \"%s\"" node.Description (newPath.Format pathFormat)
        | DeletedItem (node, false) -> sprintf "Sent %s to Recycle Bin" node.Description
        | DeletedItem (node, true) -> sprintf "Deleted %s" node.Description
    let actionComplete action pathFormat =
        actionCompleteMessage action pathFormat |> Message

    let private cannotUseNameAlreadyExists actionName (nodeType: NodeType) name hidden =
        let append = if hidden then " (hidden)" else ""
        ErrorMessage <| sprintf "Cannot %s %O \"%s\" because an item with that name already exists%s"
                            actionName nodeType name append
    let cannotCreateAlreadyExists = cannotUseNameAlreadyExists "create"
    let cannotRenameAlreadyExists = cannotUseNameAlreadyExists "rename"
    let cannotRecycle (node: Node) =
        ErrorMessage <| sprintf "Cannot move %s to the recycle bin because it is too large" node.Description
    let cannotMoveToSameFolder = ErrorMessage <| "Cannot move item to same folder it is already in"
    let cancelled = Message <| "Cancelled"

    let setActionExceptionStatus action ex (model: MainModel) =
        let actionName =
            match action with
            | CreatedItem node -> sprintf "create %s" node.Description
            | RenamedItem (node, newName) -> sprintf "rename %s" node.Description
            | MovedItem (node, newPath) -> sprintf "move %s to \"%s\"" node.Description (newPath.Format model.PathFormat)
            | CopiedItem (node, newPath) -> sprintf "copy %s to \"%s\"" node.Description (newPath.Format model.PathFormat)
            | DeletedItem (node, false) -> sprintf "recycle %s" node.Description
            | DeletedItem (node, true) -> sprintf "delete %s" node.Description
        model.Status <- Some <| StatusType.fromExn actionName ex

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

    let noUndoActions = ErrorMessage "No more actions to undo"
    let noRedoActions = ErrorMessage "No more actions to redo"
    let cannotUndoNonEmptyCreated (node: Node) =
        ErrorMessage <| sprintf "Cannot undo creation of %s because it is no longer empty" node.Description
    let cannotUndoMoveToExisting node =
        ErrorMessage <| sprintf "Cannot undo move of %s because an item exists in its previous location" node.Name
    let cannotUndoDelete permanent (node: Node) =
        ErrorMessage <| 
            if permanent then
                sprintf "Cannot undo deletion of %s" node.Description
            else
                sprintf "Cannot undo recycling of %s. Please open the Recycle Bin in Windows Explorer to restore this item" node.Description
