﻿namespace Koffee

open System.ComponentModel
open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open FSharp.Desktop.UI
open ModelExtensions
open ControlExtensions

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow, keyBindings: (KeyCombo * MainEvents) list) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    let mutable currBindings = keyBindings

    let onKey key action (evt: KeyEventArgs) =
        if evt.Key = key then
            evt.Handled <- true
            action() |> ignore

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

        // bind the path box
        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        pathBinding.Mode <- BindingMode.OneWay
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

        // simple bindings
        Binding.OfExpression
            <@
                window.NodeGrid.ItemsSource <- model.Nodes
                window.NodeGrid.SelectedIndex <- model.Cursor
                window.StatusLabel.Content <- model.Status
                window.CommandBox.Text <- model.CommandText |> BindingOptions.UpdateSourceOnChange
            @>

        // bind to the command input mode to update the UI
        model.OnPropertyChanged <@ model.CommandInputMode @> this.CommandInputModeChanged

        // make sure selected item gets set to the cursor
        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

        window.PathBox.PreviewKeyDown.Add (onKey Key.Tab window.NodeGrid.Focus)
        window.NodeGrid.PreviewKeyDown.Add (onKey Key.Tab (fun () ->
            window.PathBox.CaretIndex <- window.PathBox.Text.Length
            window.PathBox.Focus()))

        // on selection change, keep selected node in view, make sure node list is focused
        window.NodeGrid.SelectionChanged.Add (fun _ ->
            this.KeepSelectedInView()
            if not window.NodeGrid.IsFocused then
                window.NodeGrid.Focus() |> ignore)

        // on resize, keep selected node in view, update the page size when form is resized
        window.NodeGrid.SizeChanged.Add (fun _ ->
            this.KeepSelectedInView()
            match this.ItemsPerPage with
            | Some i -> model.PageSize <- i
            | None -> ())

        // escape always resets the input mode
        window.PreviewKeyDown.Add (onKey Key.Escape (fun () ->
            model.Status <- ""
            model.CommandInputMode <- None))

        window.CommandBox.LostFocus.Add (fun _ -> model.CommandInputMode <- None)

    override this.EventStreams = [
        window.PathBox.PreviewKeyDown |> onKeyFunc Key.Enter (fun () -> OpenPath window.PathBox.Text)

        window.NodeGrid.KeyDown |> Observable.choose this.TriggerKeyBindings

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
            | [ ([], newEvent) ] ->
                evt.Handled <- true
                currBindings <- keyBindings
                Some newEvent
            | [] ->
                currBindings <- keyBindings
                None
            | matchedBindings ->
                evt.Handled <- true
                currBindings <- matchedBindings
                None

    member this.CommandInputModeChanged mode =
        match mode with
        | Some inputMode -> this.ShowCommandBar (sprintf "%s:" inputMode.Name)
        | None -> this.HideCommandBar ()

    member this.ShowCommandBar label =
        window.StatusLabel.Visibility <- Visibility.Hidden
        window.CommandLabel.Content <- label
        window.CommandPanel.Visibility <- Visibility.Visible
        window.CommandBox.Focus() |> ignore

    member this.HideCommandBar () =
        window.CommandPanel.Visibility <- Visibility.Hidden
        window.StatusLabel.Visibility <- Visibility.Visible
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
