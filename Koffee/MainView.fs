namespace Koffee

open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open FSharp.Desktop.UI

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow, keyBindings: (KeyCombo * MainEvents) list) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    let mutable currBindings = keyBindings

    override this.SetBindings (model: MainModel) =
        Binding.OfExpression <@
            window.NodeList.ItemsSource <- model.Nodes
            window.NodeList.SelectedIndex <- model.Cursor
        @>

        this.AddColumn ("Name", widthWeight = 3.0)
        this.AddColumn ("Type", converter = ValueConverters.UnionText())
        this.AddColumn ("Modified", converter = ValueConverters.OptionValue(), format = "yyyy-MM-dd  HH:mm")
        this.AddColumn ("SizeFormatted", "Size", alignRight = true)

        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

        let desiredCursor = model.Cursor
        model.Cursor <- -1
        window.Loaded.Add (fun _ -> model.Cursor <- desiredCursor)

        window.PathBox.KeyDown.Add (fun e ->
            if e.Key = Key.Enter then window.NodeList.Focus() |> ignore; e.Handled <- true)
        window.NodeList.SelectionChanged.Add (fun _ ->
            if window.NodeList.SelectedItem <> null then
                window.NodeList.ScrollIntoView(window.NodeList.SelectedItem)
            if not window.NodeList.IsFocused then
                window.NodeList.Focus() |> ignore)

        window.NodeList.SizeChanged.Add (fun _ ->
            if not model.Nodes.IsEmpty then model.PageSize <- this.ItemsPerPage)

    member this.AddColumn (propName, ?header: string, ?widthWeight, ?alignRight, ?converter: IValueConverter, ?format: string) =
        let headerStr = defaultArg header propName
        let width = defaultArg widthWeight 1.0

        let col = DataGridTextColumn()
        col.Header <- headerStr
        col.Width <- DataGridLength(width, DataGridLengthUnitType.Star)
        if alignRight = Some true then
            col.ElementStyle <- Style(typedefof<TextBlock>)
            col.ElementStyle.Setters.Add(Setter(FrameworkElement.HorizontalAlignmentProperty, HorizontalAlignment.Right))

        let binding = Binding(propName)
        if converter.IsSome then binding.Converter <- converter.Value
        if format.IsSome then binding.StringFormat <- format.Value
        col.Binding <- binding

        window.NodeList.Columns.Add col

    override this.EventStreams = [
        window.PathBox.LostFocus |> Observable.mapTo PathChanged
        window.PathBox.TextChanged |> Observable.choose this.PathChangedOutside

        window.NodeList.KeyDown |> Observable.choose this.ListKeyEvent
    ]

    member this.PathChangedOutside evt =
        if not window.PathBox.IsFocused then Some PathChanged else None

    member this.ListKeyEvent evt =
        let modifiers = [
            Key.LeftShift; Key.RightShift; Key.LeftCtrl; Key.RightCtrl;
            Key.LeftAlt; Key.RightAlt; Key.LWin; Key.RWin; Key.System
        ]
        if Seq.contains evt.Key modifiers then
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

                newEvent

    member this.ItemsPerPage =
        let index = window.NodeList.SelectedIndex |> max 0
        let row = window.NodeList.ItemContainerGenerator.ContainerFromIndex(index) :?> DataGridRow
        window.NodeList.ActualHeight / row.ActualHeight |> int
