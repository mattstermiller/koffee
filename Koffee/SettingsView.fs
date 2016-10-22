namespace Koffee

open System.Windows
open System.Windows.Data
open System.Windows.Input
open FSharp.Desktop.UI
open ControlExtensions

type SettingsWindow = FsXaml.XAML<"SettingsWindow.xaml">

type SettingsView(window: SettingsWindow) =
    inherit View<SettingsEvents, SettingsModel, SettingsWindow>(window)

    let onKey key action (evt: KeyEventArgs) =
        if evt.Key = key then
            evt.Handled <- true
            action() |> ignore

    override this.SetBindings (model: SettingsModel) =
        Binding.OfExpression
            <@
                window.KeyBindings.ItemsSource <- model.KeyBindings
            @>

        window.KeyBindings.AddColumn("EventName", "Command", widthWeight = 3.0)
        window.KeyBindings.AddColumn("BoundKeys", "Bound Keys")

        window.PreviewKeyDown.Add (onKey Key.Escape (fun () -> window.Close()))

    override this.EventStreams = []
