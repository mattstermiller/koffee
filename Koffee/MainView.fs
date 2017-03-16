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

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow, keyBindings: (KeyCombo * MainEvents) list) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    let mutable currBindings = keyBindings

    let onKeyFunc key resultFunc (keyEvent : IEvent<KeyEventHandler, KeyEventArgs>) =
        keyEvent |> Observable.choose (fun evt ->
            if evt.Key = key then
                evt.Handled <- true
                Some (resultFunc())
            else
                None)

    override this.SetBindings (model: MainModel) =
        // setup grid
        window.NodeGrid.AddColumn("Name", widthWeight = 3.0)
        window.NodeGrid.AddColumn("Type", converter = ValueConverters.UnionText())
        window.NodeGrid.AddColumn("Modified", converter = ValueConverters.OptionValue(), format = "yyyy-MM-dd  HH:mm")
        window.NodeGrid.AddColumn("SizeFormatted", "Size", alignRight = true)

        // simple bindings
        Binding.OfExpression
            <@
                window.NodeGrid.ItemsSource <- model.Nodes
                window.NodeGrid.SelectedIndex <- model.Cursor
                window.CommandBox.Text <- model.CommandText |> BindingOptions.UpdateSourceOnChange
            @>

        let displayPath x =
            window.PathBox.Text <- model.PathFormatted
            window.Title <-
                model.Path.Name
                |> Str.ifEmpty model.PathFormatted
                |> sprintf "%s | Koffee"
        displayPath()
        model.OnPropertyChanged <@ model.Path @> displayPath
        model.OnPropertyChanged <@ model.PathFormat @> displayPath

        window.BufferLabel.Content <- ""
        model.OnPropertyChanged <@ model.ItemBuffer @> (fun buffer ->
            let text = MainView.BufferStatus buffer
            window.BufferLabel.Content <- text
            window.BufferLabel.Visibility <- if text = "" then Visibility.Hidden else Visibility.Visible)

        model.OnPropertyChanged <@ model.CommandTextSelection @> (fun (start, len) ->
            window.CommandBox.Select(start, len))

        // bind to the command input mode to update the UI
        model.OnPropertyChanged <@ model.CommandInputMode @> (fun mode ->
            this.CommandInputModeChanged mode model.SelectedNode
            if mode.IsNone then model.CommandTextSelection <- (999, 0))

        // update status label color
        model.OnPropertyChanged <@ model.Status @> (fun status ->
            window.StatusLabel.Content <- 
                match status with
                | Some (Message msg) | Some (ErrorMessage msg) | Some (Busy msg) -> msg
                | None -> ""
            window.StatusLabel.Foreground <-
                match status with
                | Some (ErrorMessage _) -> Brushes.Red
                | _ -> SystemColors.WindowTextBrush
        )

        // bind tab to switching focus
        window.PathBox.PreviewKeyDown.Add (onKey Key.Tab window.NodeGrid.Focus)
        window.NodeGrid.PreviewKeyDown.Add (onKey Key.Tab (fun () ->
            window.PathBox.SelectAll()
            window.PathBox.Focus()))

        // on selection change, keep selected node in view, make sure node list is focused
        window.NodeGrid.SelectionChanged.Add (fun _ ->
            this.KeepSelectedInView()
            if not window.NodeGrid.IsFocused then
                window.NodeGrid.Focus() |> ignore)

        // on resize, keep selected node in view, update the page size
        window.NodeGrid.SizeChanged.Add (fun _ ->
            this.KeepSelectedInView()
            match this.ItemsPerPage with
                | Some i -> model.PageSize <- i
                | None -> ())

        // escape and lost focus resets the input mode
        window.PreviewKeyDown.Add (onKey Key.Escape (fun _ ->
            model.Status <- None
            model.CommandInputMode <- None))
        window.CommandBox.LostFocus.Add (fun _ -> model.CommandInputMode <- None)

        // make sure selected item gets set to the cursor
        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

    override this.EventStreams = [
        window.PathBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> OpenPath window.PathBox.Text)

        window.NodeGrid.PreviewKeyDown |> Observable.choose this.TriggerKeyBindings

        window.CommandBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> ExecuteCommand)
        window.CommandBox.PreviewTextInput |> Observable.choose this.CommandTextInput
    ]

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

    member this.CommandInputModeChanged mode node =
        match mode with
        | Some inputMode -> this.ShowCommandBar (inputMode.Prompt node)
        | None -> this.HideCommandBar ()

    member this.ShowCommandBar label =
        window.CommandLabel.Content <- label
        window.CommandPanel.Visibility <- Visibility.Visible
        window.CommandBox.Focus() |> ignore

    member this.HideCommandBar () =
        window.CommandPanel.Visibility <- Visibility.Hidden
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

    static member BufferStatus buffer =
        match buffer with
        | Some (node, action) -> sprintf "%A %A: %s" action node.Type node.Name
        | None -> ""
