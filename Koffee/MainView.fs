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

    override this.SetBindings (model: MainModel) =
        // bind the path box
        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        pathBinding.Mode <- BindingMode.OneWay
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

        // bind to and setup node list/grid
        Binding.OfExpression <@
            window.NodeList.ItemsSource <- model.Nodes
            window.NodeList.SelectedIndex <- model.Cursor
            window.StatusLabel.Content <- model.Status
        @>

        window.NodeList.AddColumn("Name", widthWeight = 3.0)
        window.NodeList.AddColumn ("Type", converter = ValueConverters.UnionText())
        window.NodeList.AddColumn ("Modified", converter = ValueConverters.OptionValue(), format = "yyyy-MM-dd  HH:mm")
        window.NodeList.AddColumn ("SizeFormatted", "Size", alignRight = true)

        // make sure selected item gets set to the cursor
        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

        window.PathBox.PreviewKeyDown.Add (onKey Key.Tab window.NodeList.Focus)
        window.NodeList.PreviewKeyDown.Add (onKey Key.Tab window.PathBox.Focus)

        // on selection change, keep selected node in view, make sure node list is focused
        window.NodeList.SelectionChanged.Add (fun _ ->
            this.KeepSelectedInView()
            if not window.NodeList.IsFocused then
                window.NodeList.Focus() |> ignore)

        // on resize, keep selected node in view, update the page size when form is resized
        window.NodeList.SizeChanged.Add (fun _ ->
            this.KeepSelectedInView()
            match this.ItemsPerPage with
            | Some i -> model.PageSize <- i
            | None -> ())

    override this.EventStreams = [
        window.PathBox.PreviewKeyDown |> Observable.choose this.OpenPathOnEnter

        window.PreviewTextInput |> Observable.choose this.TriggerForInputMode
        window.NodeList.KeyDown |> Observable.choose this.TriggerKeyBindings
    ]

    member this.OpenPathOnEnter evt =
        if evt.Key = Key.Enter then
            evt.Handled <- true
            window.NodeList.Focus() |> ignore
            Some (OpenPath window.PathBox.Text)
        else
            None

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
                    | StartInput inputType -> inputMode <- Some inputType
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

    member this.KeepSelectedInView () =
        if window.NodeList.SelectedItem <> null then
            window.NodeList.ScrollIntoView(window.NodeList.SelectedItem)

    member this.ItemsPerPage =
        if window.NodeList.HasItems then
            let index = window.NodeList.SelectedIndex |> max 0
            let row = window.NodeList.ItemContainerGenerator.ContainerFromIndex(index) :?> DataGridRow
            window.NodeList.ActualHeight / row.ActualHeight |> int |> Some
        else
            None
