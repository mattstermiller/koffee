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

    override this.SetBindings (model: MainModel) =
        // bind the path box
        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

        // bind to and setup node list/grid
        Binding.OfExpression <@
            window.NodeList.ItemsSource <- model.Nodes
            window.NodeList.SelectedIndex <- model.Cursor
        @>

        window.NodeList.AddColumn("Name", widthWeight = 3.0)
        window.NodeList.AddColumn ("Type", converter = ValueConverters.UnionText())
        window.NodeList.AddColumn ("Modified", converter = ValueConverters.OptionValue(), format = "yyyy-MM-dd  HH:mm")
        window.NodeList.AddColumn ("SizeFormatted", "Size", alignRight = true)

        // make sure selected item gets set to the cursor
        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

        // Enter in path box selects node list
        window.PathBox.KeyDown.Add (fun e ->
            if e.Key = Key.Enter then
                window.NodeList.Focus() |> ignore
                e.Handled <- true)

        // keep selected node in view, make sure node list is focused after any selection change
        window.NodeList.SelectionChanged.Add (fun _ ->
            if window.NodeList.SelectedItem <> null then
                window.NodeList.ScrollIntoView(window.NodeList.SelectedItem)
            if not window.NodeList.IsFocused then
                window.NodeList.Focus() |> ignore)

        // update the page size when form is resized
        window.NodeList.SizeChanged.Add (fun _ ->
            match this.ItemsPerPage with
            | Some i -> model.PageSize <- i
            | None -> ())

    override this.EventStreams = [
        window.PathBox.LostFocus |> Observable.mapTo PathChanged
        window.PathBox.TextChanged |> Observable.choose this.PathChangedOutside

        window.PreviewTextInput |> Observable.choose this.PreviewTextInput
        window.NodeList.KeyDown |> Observable.choose this.ListKeyEvent
    ]

    member this.PathChangedOutside evt =
        if not window.PathBox.IsFocused then Some PathChanged else None

    member this.PreviewTextInput evt =
        match (inputMode, evt.Text.ToCharArray()) with
        | (Some FindInput, [| c |]) ->
            evt.Handled <- true
            inputMode <- None
            Some (Find c)
        | _ -> None

    member this.ListKeyEvent evt =
        let modifiers = [
            Key.LeftShift; Key.RightShift; Key.LeftCtrl; Key.RightCtrl;
            Key.LeftAlt; Key.RightAlt; Key.LWin; Key.RWin; Key.System
        ]
        if inputMode.IsSome || Seq.contains evt.Key modifiers then
            None
        else
            let chord = (Keyboard.Modifiers, evt.Key)
            // choose bindings where the next key/chord matches what was just pressed, with the remaining keys
            let matchBindings =
                currBindings
                |> List.choose
                    (fun (keyCombo, boundEvt) ->
                        match keyCombo with
                        | kc :: rest when kc = chord-> Some (rest, boundEvt)
                        | _ -> None)

            if matchBindings.IsEmpty then
                // if none matched, reset the bindings
                currBindings <- keyBindings
                None
            else
                evt.Handled <- true
                // within the matching bindings, find the last one that has no more keys and get its event
                let newEvent =
                    matchBindings
                    |> List.choose
                        (fun (keyCombo, evt) ->
                            match keyCombo with
                            | [] -> Some evt
                            | _ -> None)
                    |> List.tryLast

                if newEvent.IsSome then
                    // if there is an event to return, reset the bindings
                    currBindings <- keyBindings
                else
                    // otherwise the bindings need more key presses to match
                    // set the current bindings to the filtered list
                    currBindings <- matchBindings

                // intercept StartInput events
                match newEvent with
                | Some (StartInput inputType) -> inputMode <- Some inputType
                | _ -> ()

                newEvent

    member this.ItemsPerPage =
        if window.NodeList.HasItems then
            let index = window.NodeList.SelectedIndex |> max 0
            let row = window.NodeList.ItemContainerGenerator.ContainerFromIndex(index) :?> DataGridRow
            window.NodeList.ActualHeight / row.ActualHeight |> int |> Some
        else
            None
