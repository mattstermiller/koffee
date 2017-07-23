namespace Koffee

open System
open System.Windows
open System.Windows.Data
open System.Windows.Input
open FSharp.Desktop.UI
open UIHelpers
open ConfigExt
open System.Windows.Controls.Primitives

type SettingsWindow = FsXaml.XAML<"SettingsWindow.xaml">

type SettingsView(window: SettingsWindow, config: Config) =
    inherit View<SettingsEvents, SettingsModel, SettingsWindow>(window)

    override this.SetBindings (model: SettingsModel) =
        let check (ctl: ToggleButton) =
            ctl.IsChecked <- Nullable true

        check (if config.StartupPath = RestorePrevious then window.StartupPathPrevious else window.StartupPathDefault)
        check (if config.PathFormat = Windows then window.PathFormatWindows else window.PathFormatUnix)
        window.DefaultPath.Text <- config.DefaultPath

        if config.Window.ShowFullPathInTitle then
            check window.ShowFullPathInTitleBar
        if config.ShowHidden then
            check window.ShowHidden

        Binding.OfExpression
            <@
                window.KeyBindings.ItemsSource <- model.KeyBindings
            @>

        window.KeyBindings.AddColumn("EventName", "Command", widthWeight = 3.0)
        window.KeyBindings.AddColumn("BoundKeys", "Bound Keys")

        window.PreviewKeyDown.Add (onKey Key.Escape window.Close)
        window.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W window.Close)

    override this.EventStreams = [
        window.StartupPathPrevious.Checked |> Observable.mapTo (StartupPathChanged RestorePrevious)
        window.StartupPathDefault.Checked |> Observable.mapTo (StartupPathChanged DefaultPath)
        window.DefaultPath.LostFocus |> Observable.map (fun _ -> DefaultPathChanged (window.DefaultPath.Text))

        window.PathFormatWindows.Checked |> Observable.mapTo (PathFormatChanged Windows)
        window.PathFormatUnix.Checked |> Observable.mapTo (PathFormatChanged Unix)

        window.ShowFullPathInTitleBar.Checked |> Observable.mapTo (ShowFullPathInTitleChanged true)
        window.ShowFullPathInTitleBar.Unchecked |> Observable.mapTo (ShowFullPathInTitleChanged false)

        window.ShowHidden.Checked |> Observable.mapTo (ShowHiddenChanged true)
        window.ShowHidden.Unchecked |> Observable.mapTo (ShowHiddenChanged false)
    ]
