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

        window.TextEditor.Text <- config.TextEditor

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

    override this.EventStreams =
        let textBoxChanged evt (textBox: Controls.TextBox) =
            textBox.LostFocus |> Observable.map (fun _ -> evt (textBox.Text))

        let checkedChanged evt (checkBox: Controls.CheckBox) =
            (checkBox.Checked |> Observable.mapTo (evt true),
             checkBox.Unchecked |> Observable.mapTo (evt false))
            ||> Observable.merge

        [ window.StartupPathPrevious.Checked |> Observable.mapTo (StartupPathChanged RestorePrevious)
          window.StartupPathDefault.Checked |> Observable.mapTo (StartupPathChanged DefaultPath)
          window.DefaultPath |> textBoxChanged DefaultPathChanged

          window.TextEditor |> textBoxChanged TextEditorChanged

          window.PathFormatWindows.Checked |> Observable.mapTo (PathFormatChanged Windows)
          window.PathFormatUnix.Checked |> Observable.mapTo (PathFormatChanged Unix)

          window.ShowFullPathInTitleBar |> checkedChanged ShowFullPathInTitleChanged
          window.ShowHidden |> checkedChanged ShowHiddenChanged
        ]
