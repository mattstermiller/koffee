namespace Koffee

open System.Windows
open System.Windows.Data
open System.Windows.Input
open FSharp.Desktop.UI
open UIHelpers

type SettingsWindow = FsXaml.XAML<"SettingsWindow.xaml">

type SettingsView(window: SettingsWindow) =
    inherit View<SettingsEvents, SettingsModel, SettingsWindow>(window)

    override this.SetBindings (model: SettingsModel) =
        Binding.OfExpression
            <@
                window.KeyBindings.ItemsSource <- model.KeyBindings
            @>

        window.KeyBindings.AddColumn("EventName", "Command", widthWeight = 3.0)
        window.KeyBindings.AddColumn("BoundKeys", "Bound Keys")

        window.PreviewKeyDown.Add (onKey Key.Escape window.Close)
        window.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W window.Close)

    override this.EventStreams = []
