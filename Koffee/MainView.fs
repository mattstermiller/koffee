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

        window.NodeList.Focus() |> ignore

    member this.AddColumn (propName, ?header: string, ?widthWeight, ?alignRight, ?converter: IValueConverter, ?format: string) =
        let headerStr = defaultArg header propName
        let width = defaultArg widthWeight 1.0

        let col = DataGridTextColumn()
        col.Header <- headerStr
        col.Width <- DataGridLength(width, DataGridLengthUnitType.Star)
        if alignRight = Some true then
            col.CellStyle <- Style(typedefof<DataGridCell>)
            col.CellStyle.Setters.Add(Setter(FrameworkElement.HorizontalAlignmentProperty, HorizontalAlignment.Right))

        let binding = Binding(propName)
        if converter.IsSome then binding.Converter <- converter.Value
        if format.IsSome then binding.StringFormat <- format.Value
        col.Binding <- binding

        window.NodeList.Columns.Add col


    override this.EventStreams = [
        window.PathBox.KeyDown |> Observable.choose this.PathKeyEvent
        window.PathBox.LostFocus |> Observable.mapTo PathChanged
        window.PathBox.TextChanged |> Observable.choose this.PathChangedOutside

        window.NodeList.KeyDown |> Observable.choose this.ListKeyEvent
        window.NodeList.SelectionChanged |> Observable.choose this.FocusNodeList
    ]

    member this.FocusNodeList evt : MainEvents option =
        if not window.NodeList.IsFocused then window.NodeList.Focus() |> ignore
        None

    member this.PathChangedOutside evt =
        if not window.PathBox.IsFocused then Some PathChanged else None

    member this.PathKeyEvent evt =
        if Keyboard.Modifiers = ModifierKeys.None then
            evt.Handled <- true
            match evt.Key with
            | Key.Enter -> window.NodeList.Focus() |> ignore; None
            | _ -> evt.Handled <- false; None
        else None

    member this.ListKeyEvent evt =
        if Keyboard.Modifiers = ModifierKeys.None then
            evt.Handled <- true
            match evt.Key with
            | Key.J -> Some (Nav 1)
            | Key.K -> Some (Nav -1)
            | Key.H -> Some OpenParent
            | Key.L -> Some OpenSelected
            | _ -> evt.Handled <- false; None
        else if Keyboard.Modifiers = ModifierKeys.Control then
            evt.Handled <- true
            match evt.Key with
            | Key.E -> Some OpenExplorer
            | _ -> evt.Handled <- false; None
        else None

