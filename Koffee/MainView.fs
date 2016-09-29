namespace Koffee

open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open FSharp.Desktop.UI

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    member this.AddColumn (propName, ?converter: IValueConverter) =
        let col = DataGridTextColumn()
        window.NodeList.Columns.Add col

        let binding = Binding(propName)
        if converter.IsSome then
            binding.Converter <- converter.Value
        col.Binding <- binding

    override this.SetBindings (model: MainModel) =
        Binding.OfExpression <@
            window.NodeList.ItemsSource <- model.Nodes
            window.NodeList.SelectedIndex <- model.Cursor
        @>

        this.AddColumn "Name"
        this.AddColumn ("Type", ValueConverters.UnionText())

        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

        window.NodeList.Focus() |> ignore

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
            | Key.J -> Some NavDown
            | Key.K -> Some NavUp
            | Key.H -> Some OpenParent
            | Key.L -> Some OpenSelected
            | _ -> evt.Handled <- false; None
        else None

