namespace Koffee

open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open FSharp.Desktop.UI
open ControlExtensions

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow, keyBindings: (KeyCombo * MainEvents) list) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    let mutable currBindings = keyBindings
    let mutable inputMode : CommandInput option = None

    let onKey key action (evt: KeyEventArgs) =
        if evt.Key = key then
            evt.Handled <- true
            action() |> ignore

    let chooseOnKey key resultFunc (keyEvent : IEvent<KeyEventHandler, KeyEventArgs>) =
        keyEvent |> Observable.choose (fun evt ->
            if evt.Key = key then
                evt.Handled <- true
                resultFunc()
            else
                None)
    override this.SetBindings (model: MainModel) =
        // bind the path box
        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        pathBinding.Mode <- BindingMode.OneWay
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

        // bind to and setup node list/grid
        Binding.OfExpression
            <@
                window.NodeGrid.ItemsSource <- model.Nodes
                window.NodeGrid.SelectedIndex <- model.Cursor
                window.StatusLabel.Content <- model.Status
            @>

        window.NodeGrid.AddColumn("Name", widthWeight = 3.0)
        window.NodeGrid.AddColumn("Type", converter = ValueConverters.UnionText())
        window.NodeGrid.AddColumn("Modified", converter = ValueConverters.OptionValue(), format = "yyyy-MM-dd  HH:mm")
        window.NodeGrid.AddColumn("SizeFormatted", "Size", alignRight = true)

        // make sure selected item gets set to the cursor
        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

        window.PathBox.PreviewKeyDown.Add (onKey Key.Tab window.NodeGrid.Focus)
        window.NodeGrid.PreviewKeyDown.Add
            (onKey Key.Tab (fun () ->
                                window.PathBox.CaretIndex <- window.PathBox.Text.Length
                                window.PathBox.Focus()))

        // on selection change, keep selected node in view, make sure node list is focused
        window.NodeGrid.SelectionChanged.Add (fun _ ->
            this.KeepSelectedInView()
            if not window.NodeGrid.IsFocused then
                window.NodeGrid.Focus() |> ignore)

        // escape always resets the input mode
        window.PreviewKeyDown.Add (onKey Key.Escape (fun () ->
            model.Status <- ""
            this.SetInputMode None))

        // on resize, keep selected node in view, update the page size when form is resized
        window.NodeGrid.SizeChanged.Add (fun _ ->
            this.KeepSelectedInView()
            match this.ItemsPerPage with
            | Some i -> model.PageSize <- i
            | None -> ())

        window.CommandBox.LostFocus.Add (fun _ -> this.SetInputMode None)

    override this.EventStreams = [
        window.PathBox.PreviewKeyDown |> chooseOnKey Key.Enter (fun () -> Some (OpenPath window.PathBox.Text))

        window.PreviewTextInput |> Observable.choose this.TriggerForInputMode
        window.NodeGrid.KeyDown |> Observable.choose this.TriggerKeyBindings

        window.CommandBox.PreviewKeyDown |> chooseOnKey Key.Enter this.GetCommandEvent
    ]

    member this.GetCommandEvent () =
        match inputMode with
        | Some SearchInput ->
            let search = window.CommandBox.Text
            window.CommandBox.Text <- ""
            Some (Search search)
        | Some FindInput -> None
        | None -> None

    member this.TriggerForInputMode keyEvt =
        match (inputMode, keyEvt.Text.ToCharArray()) with
        | (Some FindInput, [| c |]) ->
            keyEvt.Handled <- true
            inputMode <- None
            Some (Find c)
        | _ -> None

    member this.TriggerKeyBindings evt =
        let modifierKeys = [
            Key.LeftShift; Key.RightShift; Key.LeftCtrl; Key.RightCtrl;
            Key.LeftAlt; Key.RightAlt; Key.LWin; Key.RWin; Key.System
        ]
        let chord = (Keyboard.Modifiers, evt.Key)

        if inputMode.IsSome || Seq.contains evt.Key modifierKeys then
            None
        else if chord = (ModifierKeys.None, Key.Escape) then
            evt.Handled <- true
            currBindings <- keyBindings
            None
        else
            match KeyBinding.GetMatch currBindings chord with
            | [ ([], newEvent) ] ->
                // if StartInput event, update mode
                match newEvent with
                    | StartInput inputType -> this.SetInputMode (Some inputType)
                    | _ -> ()
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

    member this.SetInputMode inputType =
        match inputType with
        | Some SearchInput -> this.ShowCommandBar "Search:"
        | None -> this.HideCommandBar ()
        | _ -> ()
        inputMode <- inputType

    member this.ShowCommandBar label =
        let setLeftMargin left (control: Control) =
            let m = control.Margin
            control.Margin <- Thickness(left, m.Top, m.Right, m.Bottom)

        window.StatusLabel.Visibility <- Visibility.Hidden

        window.CommandLabel.Content <- label
        window.CommandLabel.Visibility <- Visibility.Visible
        window.CommandLabel.UpdateLayout()

        setLeftMargin window.CommandLabel.ActualWidth window.CommandBox
        window.CommandBox.Visibility <- Visibility.Visible
        window.CommandBox.Focus() |> ignore

    member this.HideCommandBar () =
        window.StatusLabel.Visibility <- Visibility.Visible
        window.CommandLabel.Visibility <- Visibility.Hidden
        window.CommandBox.Visibility <- Visibility.Hidden
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
