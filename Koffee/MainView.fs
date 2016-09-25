namespace Koffee

open System.Windows.Data
open System.Windows.Input
open System.Windows.Controls
open FSharp.Desktop.UI

type MainWindow = FsXaml.XAML<"MainWindow.xaml">

type MainView(window: MainWindow) =
    inherit View<MainEvents, MainModel, MainWindow>(window)

    let addCol propName =
        let col = DataGridTextColumn()
        col.Binding <- Binding(propName)
        window.NodeList.Columns.Add col

    override this.SetBindings (model: MainModel) =
        Binding.OfExpression <@
            window.NodeList.ItemsSource <- model.Nodes
            window.NodeList.SelectedIndex <- model.Cursor
        @>

        window.NodeList.AutoGenerateColumns <- false
        addCol "Name"
        addCol "Type"

        let pathBinding = Binding("Path")
        pathBinding.Converter <- ValueConverters.UnionValue()
        window.PathBox.SetBinding(TextBox.TextProperty, pathBinding) |> ignore

    override this.EventStreams = [
        window.PathBox.TextChanged |> Observable.map (fun _ -> PathChanged)
        window.KeyDown
            |> Observable.map this.KeyToEvent
            |> Observable.filter (fun e -> e.IsSome)
            |> Observable.map (fun e -> e.Value)
    ]

    member this.KeyToEvent evt : MainEvents option =
        if Keyboard.Modifiers = ModifierKeys.None then
            evt.Handled <- true
            match evt.Key with
            | System.Windows.Input.Key.J -> Some NavDown
            | System.Windows.Input.Key.K -> Some NavUp
            | System.Windows.Input.Key.H -> Some OpenParent
            | System.Windows.Input.Key.L -> Some OpenSelected
            | _ -> evt.Handled <- false; None
        else None

