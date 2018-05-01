namespace Koffee

open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Media
open FSharp.Desktop.UI
open System.Reactive.Linq
open ModelExtensions
open UIHelpers
open Utility
open Reflection
open ConfigExt

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow,
              keyBindings: (KeyCombo * MainEvents) list,
              config: Config,
              startupOptions: StartupOptions) =
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
        window.NodeGrid.AddColumn("Type")
        window.NodeGrid.AddColumn("Modified", converter = ValueConverters.OptionValue(), format = FormatString.dateTime)
        window.NodeGrid.AddColumn("SizeFormatted", "Size", alignRight = true)
        window.NodeGrid.Columns |> Seq.iter (fun c -> c.CanUserSort <- false)

        // simple bindings
        Binding.OfExpression
            <@
                window.NodeGrid.ItemsSource <- model.Nodes
                window.NodeGrid.SelectedIndex <- model.Cursor
                window.InputBox.Text <- model.InputText |> BindingOptions.UpdateSourceOnChange
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

        // display and save register
        bindPropertyToFunc <@ model.YankRegister @> (fun register ->
            let configRegister = register |> Option.map (fun (node, action) -> node.Path, action)
            if configRegister <> config.YankRegister then
                config.YankRegister <- configRegister
                config.Save()
            let text =
                register |> Option.map (fun (node, action) ->
                    sprintf "%A %A: %s" action node.Type node.Name)
            window.Dispatcher.Invoke (fun () ->
                window.RegisterText.Text <- text |> Option.defaultValue ""
                window.RegisterPanel.Visible <- text.IsSome))

        // update input text selection
        bindPropertyToFunc <@ model.InputTextSelection @> (fun (start, len) ->
            window.InputBox.Select(start, len))

        // update UI for the input input mode
        bindPropertyToFunc <@ model.InputMode @> (fun mode ->
            this.InputModeChanged mode model.PathFormat model.SelectedNode
            if mode.IsNone then model.InputTextSelection <- (999, 0))

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
            model.InputMode <- None
            window.NodeGrid.Focus() |> ignore)
        window.InputBox.LostFocus.Add (fun _ -> model.InputMode <- None)

        // on path enter, update to formatted path and focus grid
        window.Loaded.Add (fun _ ->
            window.PathBox.PreviewKeyDown.Add (fun evt ->
                if evt.Key = Key.Enter && not model.HasErrorStatus then
                    evt.Handled <- true
                    window.PathBox.Text <- model.PathFormatted
                    window.NodeGrid.Focus() |> ignore))

        // suppress text input for character prompts
        window.Loaded.Add (fun _ ->
            window.InputBox.PreviewTextInput.Add (fun evt ->
                match model.InputMode with
                | Some (Prompt _) | Some (Confirm _) -> evt.Handled <- true
                | _ -> ()))

        // pressing delete when viewing bookmarks allows delete
        window.InputBox.PreviewKeyDown.Add (fun evt ->
            if evt.Key = Key.Delete then
                match model.InputMode with
                | Some (Prompt GoToBookmark) | Some (Prompt SetBookmark) ->
                    evt.Handled <- true
                    model.InputMode <- Some (Prompt DeleteBookmark)
                | _ -> ())

        // make sure selected item gets set to the cursor
        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

        // load window settings
        bindPropertyToFunc <@ model.WindowLocation @> (fun (left, top) ->
            if int window.Left <> left then window.Left <- float left
            if int window.Top <> top then window.Top <- float top)
        bindPropertyToFunc <@ model.WindowSize @> (fun (width, height) ->
            if int window.Width <> width then window.Width <- float width
            if int window.Height <> height then window.Height <- float height)
        if config.Window.IsMaximized then
            window.WindowState <- WindowState.Maximized

        // setup saving window settings
        let saveWindowSettings = startupOptions.Location.IsNone && startupOptions.Size.IsNone
        if saveWindowSettings then
            window.StateChanged.Add (fun _ -> 
                if window.WindowState <> WindowState.Minimized then
                    config.Window.IsMaximized <- window.WindowState = WindowState.Maximized
                    config.Save())
        window.LocationChanged.Throttle(System.TimeSpan.FromSeconds(0.2)).Add (fun _ ->
            window.Dispatcher.Invoke(fun () ->
                if window.Left > -window.Width && window.Top > -window.Height then
                    model.WindowLocation <- (int window.Left, int window.Top)
                    if saveWindowSettings then
                        config.Window.Left <- int window.Left
                        config.Window.Top <- int window.Top
                        config.Save()))
        window.SizeChanged.Throttle(System.TimeSpan.FromSeconds(0.2)).Add (fun _ -> 
            window.Dispatcher.Invoke(fun () ->
                model.WindowSize <- (int window.Width, int window.Height)
                if saveWindowSettings then
                    config.Window.Width <- int window.Width
                    config.Window.Height <- int window.Height
                    config.Save()))

        window.Closed.Add (fun _ ->
            config.PreviousPath <- model.Path.Format Windows
            config.Save())

    override this.EventStreams = [
        window.Activated |> Observable.choose this.Activated
        window.PathBox.PreviewKeyDown |> Observable.choose (fun evt ->
            if evt.Key = Key.Enter then Some <| OpenPath window.PathBox.Text
            else None)
        window.PathBox.PreviewKeyDown |> Observable.choose (fun evt ->
            match this.TriggerKeyBindings evt with
            | Some Exit -> Some Exit
            | _ -> evt.Handled <- false; None)
        window.SettingsButton.Click |> Observable.mapTo OpenSettings
        window.NodeGrid.PreviewKeyDown |> Observable.choose this.TriggerKeyBindings
        window.InputBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> SubmitInput)
        window.InputBox.PreviewTextInput |> Observable.choose this.InputKey
    ]

    member this.Activated _ =
        if config.Window.RefreshOnActivate then
            Some Refresh
        else None

    member this.InputKey keyEvt =
        match keyEvt.Text.ToCharArray() with
        | [| c |] -> Some (InputCharTyped c)
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
        else if chord = (ModifierKeys.Control, Key.C) then
            evt.Handled <- true // prevent crash due to bug in WPF datagrid
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
        window.StatusText.Text <- 
            match status with
            | Some (Message msg) | Some (ErrorMessage msg) | Some (Busy msg) -> msg
            | None -> ""
        window.StatusText.Foreground <-
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

    member this.InputModeChanged mode pathFormat node =
        match mode with
        | Some inputMode ->
            match inputMode with
            | Prompt GoToBookmark
            | Prompt SetBookmark
            | Prompt DeleteBookmark ->
                let bookmarks =
                    match config.GetBookmarks() with
                    | bm when bm |> Seq.isEmpty -> [(' ', "No bookmarks set")] |> dict
                    | bm -> bm
                window.Bookmarks.ItemsSource <- bookmarks
                window.BookmarkPanel.Visible <- true
            | _ ->
                window.BookmarkPanel.Visible <- false
            this.ShowInputBar (inputMode |> this.GetPrompt pathFormat node)
        | None -> this.HideInputBar ()

    member this.GetPrompt pathFormat (node: Node) =
        let caseName (case: obj) = case |> GetUnionCaseName |> Str.readableIdentifier |> sprintf "%s:"
        function
        | Confirm confirmType ->
            match confirmType with
            | Overwrite (_, src, dest) ->
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
            | Delete -> sprintf "Permanently delete %s y/n ?" node.Description
            | OverwriteBookmark (char, existingPath) ->
                sprintf "Overwrite bookmark \"%c\" currently set to \"%s\" y/n ?" char (existingPath.Format pathFormat)
        | Prompt (Find caseSensitive) -> sprintf "Find%s:" (if caseSensitive then " case-sensitive" else "")
        | Prompt promptType -> promptType |> caseName
        | Input inputType -> inputType |> caseName

    member private this.ShowInputBar label =
        window.InputText.Text <- label
        window.InputPanel.Visible <- true
        window.InputBox.Focus() |> ignore

    member private this.HideInputBar () =
        window.InputPanel.Visible <- false
        window.BookmarkPanel.Visible <- false
        window.NodeGrid.Focus() |> ignore

    member this.KeepSelectedInView () =
        if window.NodeGrid.SelectedItem <> null then
            window.NodeGrid.ScrollIntoView(window.NodeGrid.SelectedItem)

    member this.ItemsPerPage =
        if window.NodeGrid.HasItems then
            let index = window.NodeGrid.SelectedIndex |> max 0
            let row = window.NodeGrid.ItemContainerGenerator.ContainerFromIndex(index) :?> DataGridRow |> Option.ofObj
            row |> Option.map (fun row -> window.NodeGrid.ActualHeight / row.ActualHeight |> int)
        else
            None

module MainStatus =
    // navigation
    let find caseSensitive char = Message <| sprintf "Find %O%s" char (if caseSensitive then " (case-sensitive)" else "")
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
    let openExplorer = Message "Opened Windows Explorer"
    let openCommandLine path = Message <| sprintf "Opened Commandline at: %s" path
    let openTextEditor name = Message <| sprintf "Opened text editor for: %s" name
    let removedNetworkHost host = Message <| sprintf "Removed network host: %s" host

    let private runningActionMessage action pathFormat =
        match action with
        | MovedItem (node, newPath) -> Some <| sprintf "Moving %s to \"%s\"..." node.Description (newPath.Format pathFormat)
        | CopiedItem (node, newPath) -> Some <| sprintf "Copying %s to \"%s\"..." node.Description (newPath.Format pathFormat)
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
        | MovedItem (node, newPath) -> sprintf "Moved %s to \"%s\"" node.Description (newPath.Format pathFormat)
        | CopiedItem (node, newPath) -> sprintf "Copied %s to \"%s\"" node.Description (newPath.Format pathFormat)
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

type MainError =
    | ActionError of actionName: string * exn
    | ItemActionError of ItemAction * PathFormat * exn
    | InvalidPath of string
    | InvalidSearchSlash
    | InvalidSearchSwitch of char
    | CannotPutHere
    | CannotUseNameAlreadyExists of actionName: string * nodeType: NodeType * name: string * hidden: bool
    | CannotMoveToSameFolder
    | CouldNotOpenApp of app: string * exn
    | CouldNotFindKoffeeExe

    member this.Message =
        match this with
        | ActionError (action, e) ->
            let msg =
                match e with
                | :? System.AggregateException as agg -> agg.InnerExceptions.[0].Message
                | e -> e.Message
            sprintf "Could not %s: %s" action msg
        | ItemActionError (action, pathFormat, e) ->
            let actionName =
                match action with
                | CreatedItem node -> sprintf "create %s" node.Description
                | RenamedItem (node, newName) -> sprintf "rename %s" node.Description
                | MovedItem (node, newPath) -> sprintf "move %s to \"%s\"" node.Description (newPath.Format pathFormat)
                | CopiedItem (node, newPath) -> sprintf "copy %s to \"%s\"" node.Description (newPath.Format pathFormat)
                | DeletedItem (node, false) -> sprintf "recycle %s" node.Description
                | DeletedItem (node, true) -> sprintf "delete %s" node.Description
            (ActionError (actionName, e)).Message
        | InvalidPath path -> sprintf "Path format is invalid: %s" path
        | InvalidSearchSlash -> "Invalid search: only one slash \"/\" may be used. Slash is used to delimit switches."
        | InvalidSearchSwitch c -> sprintf "Invalid search switch \"%c\". Valid switches are: c, i" c
        | CannotPutHere -> "Cannot put items here"
        | CannotUseNameAlreadyExists (actionName, nodeType, name, hidden) ->
            let append = if hidden then " (hidden)" else ""
            sprintf "Cannot %s %O \"%s\" because an item with that name already exists%s"
                    actionName nodeType name append
        | CannotMoveToSameFolder -> "Cannot move item to same folder it is already in"
        | CouldNotOpenApp (app, e) -> sprintf "Could not open app %s: %s" app e.Message
        | CouldNotFindKoffeeExe -> "Could not determine Koffee.exe path"

[<AutoOpen>]
module MainModelExt =
    type MainModel with
        member this.SetError (e: MainError) =
            this.Status <- Some (ErrorMessage e.Message)

        member this.SetItemError action e =
            this.SetError <| ItemActionError (action, this.PathFormat, e)
