namespace Koffee

open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open FSharp.Desktop.UI

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

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
        if Keyboard.Modifiers = ModifierKeys.None then
            evt.Handled <- true
            match evt.Key with
            | Key.J -> Some (Nav 1)
            | Key.K -> Some (Nav -1)
            | Key.H -> Some OpenParent
            | Key.L -> Some OpenSelected
            | _ -> evt.Handled <- false; None
        else if Keyboard.Modifiers = ModifierKeys.Shift then
            evt.Handled <- true
            match evt.Key with
            | Key.G -> Some (Nav (System.Int32.MaxValue / 2))
            | _ -> evt.Handled <- false; None
        else if Keyboard.Modifiers = ModifierKeys.Control then
            evt.Handled <- true
            match evt.Key with
            | Key.E -> Some OpenExplorer
            | Key.U | Key.K -> Some (Nav -this.ItemsPerHalfPage)
            | Key.D | Key.J -> Some (Nav this.ItemsPerHalfPage)
            | _ -> evt.Handled <- false; None
        else None

    member this.ItemsPerHalfPage =
        let row = window.NodeList.ItemContainerGenerator.ContainerFromIndex(window.NodeList.SelectedIndex) :?> DataGridRow
        window.NodeList.ActualHeight / row.ActualHeight * 0.45 |> int
