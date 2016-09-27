namespace Koffee

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

        let addCol propName =
            let col = DataGridTextColumn()
            col.Binding <- Binding(propName)
            window.NodeList.Columns.Add col

        window.NodeList.AutoGenerateColumns <- false
        addCol "Name"
        addCol "Type"

        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

    override this.EventStreams = [
        window.PathBox.TextChanged |> Observable.map (fun _ -> PathChanged)
        window.NodeList.KeyDown |> Observable.choose this.ListKeyEvent
    ]

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

