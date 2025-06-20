module Koffee.Settings

open System
open System.Windows.Input
open System.Windows.Controls.Primitives
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp
open UIHelpers
open KoffeeUI
open KeyBinder

type Model = {
    Config: Config
    DefaultPath: Result<Path, string>
    CommandBindings: CommandBinding list
    KeyBindingIndex: int
}

type Events =
    | StartPathChanged of StartPath
    | DefaultPathChanged
    | PathFormatChanged of PathFormat
    | EditSearchExclusions
    | EditKeyBinding of isSecondary: bool

let describeKeyCombo (combo: KeyCombo option) =
    combo |> Option.map KeyBindingLogic.keyComboDescription |? ""

let private binder (window: SettingsWindow) model =
    window.KeyBindings.AddColumn(<@ fun k -> k.Command @>, "Command", conversion = (fun cmd -> cmd.Name), widthWeight = 3.0)
    window.KeyBindings.AddColumn(<@ fun k -> k.KeyCombo1 @>, "Key Combo 1", conversion = describeKeyCombo)
    window.KeyBindings.AddColumn(<@ fun k -> k.KeyCombo2 @>, "Key Combo 2", conversion = describeKeyCombo)

    window.PreviewKeyDown.Add (onKey Key.Escape window.Close)
    window.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W window.Close)

    let check (ctl: ToggleButton) =
        ctl.IsChecked <- Nullable true
    check (if model.Config.StartPath = RestorePrevious then window.StartPathPrevious else window.StartPathDefault)
    check (if model.Config.PathFormat = Windows then window.PathFormatWindows else window.PathFormatUnix)

    let validatePath = Path.Parse >> Result.ofOption "Invalid path format."

    [
        Bind.view(<@ window.DefaultPath.Text @>).toModelResult(<@ model.DefaultPath @>, validatePath, string)
        Bind.view(<@ window.TerminalPath.Text @>).toModel(<@ model.Config.TerminalPath @>)
        Bind.view(<@ window.TextEditor.Text @>).toModel(<@ model.Config.TextEditor @>)

        Bind.view(<@ window.ShowHidden.IsChecked @>).toModel(<@ model.Config.ShowHidden @>)
        Bind.view(<@ window.ShowNextUndoRedo.IsChecked @>).toModel(<@ model.Config.ShowNextUndoRedo @>)
        Bind.view(<@ window.ShowFullPathInTitleBar.IsChecked @>).toModel(<@ model.Config.Window.ShowFullPathInTitle @>)
        Bind.view(<@ window.RefreshOnActivate.IsChecked @>).toModel(<@ model.Config.Window.RefreshOnActivate @>)

        Bind.model(<@ model.CommandBindings @>).toItemsSourceDirect(window.KeyBindings)
        Bind.view(<@ window.KeyBindings.SelectedIndex @>).toModel(<@ model.KeyBindingIndex @>)
    ]

module Obs = Observable

let private events (window: SettingsWindow) = [
    window.StartPathPrevious.Checked |> Obs.mapTo (StartPathChanged RestorePrevious)
    window.StartPathDefault.Checked |> Obs.mapTo (StartPathChanged DefaultPath)
    window.DefaultPath.LostFocus |> Obs.mapTo DefaultPathChanged
    window.EditSearchExclusions.Click |> Obs.mapTo EditSearchExclusions
    window.PathFormatWindows.Checked |> Obs.mapTo (PathFormatChanged Windows)
    window.PathFormatUnix.Checked |> Obs.mapTo (PathFormatChanged Unix)
    window.EditKeyBinding1.Click |> Obs.mapTo (EditKeyBinding false)
    window.EditKeyBinding2.Click |> Obs.mapTo (EditKeyBinding true)
]

let updateConfig f (model: Model) =
    { model with Config = f model.Config }

let defaultPathChanged (model: Model) =
    match model.DefaultPath with
    | Ok path -> model |> updateConfig (fun c -> { c with DefaultPath = path })
    | Error _ -> model

let editSearchExclusions (textEdit: TextEdit.Dialog) (model: Model) =
    let searchExclusions =
        textEdit.Open
            "Recursive Search Exclusions"
            ("This list of folder names are excluded from recursive searches.\n" +
                "Disable exclusions by adding \"/\" in front of them.")
            (model.Config.SearchExclusions @ [""] |> String.concat "\n")
        |> String.split '\n' |> Array.toList |> List.map String.trim
    model |> updateConfig (fun c -> { c with SearchExclusions = searchExclusions })

let editKeyBinding (keyBinder: KeyBinder.Dialog) isSecondaryBinding (model: Model) =
    let bindingToEdit = model.CommandBindings |> List.item model.KeyBindingIndex
    let currentKeyCombo =
        if isSecondaryBinding
        then bindingToEdit.KeyCombo2
        else bindingToEdit.KeyCombo1
    match keyBinder.Open bindingToEdit.Command currentKeyCombo model.CommandBindings with
    | Some keyCombo ->
        let keyComboOption = keyCombo |> Option.ofCond Seq.isNotEmpty
        let newBinding =
            if isSecondaryBinding
            then { bindingToEdit with KeyCombo2 = keyComboOption }
            else { bindingToEdit with KeyCombo1 = keyComboOption }
            |> CommandBinding.normalize
        let commandBindings =
            model.CommandBindings |> List.mapi (fun i binding ->
                if i = model.KeyBindingIndex
                then newBinding
                else binding |> Option.foldBack CommandBinding.removeKeyCombo keyComboOption
            )
        { model with
            CommandBindings = commandBindings
            Config = { model.Config with KeyBindings = commandBindings |> List.collect CommandBinding.keyBindings }
        }
    | None ->
        model

let private dispatcher textEdit keyBinder evt =
    match evt with
    | StartPathChanged value -> Sync (updateConfig (fun c -> { c with StartPath = value }))
    | DefaultPathChanged -> Sync defaultPathChanged
    | PathFormatChanged value -> Sync (updateConfig (fun c -> { c with PathFormat = value}))
    | EditSearchExclusions -> Sync (editSearchExclusions textEdit)
    | EditKeyBinding isSecondary -> Sync (editKeyBinding keyBinder isSecondary)

let private start (config: Config) window =
    let keyBinding command =
        let bindings = KeyBindingLogic.getKeyCombos config.KeyBindings command
        {
            Command = command
            KeyCombo1 = bindings |> List.tryItem 0
            KeyCombo2 = bindings |> List.tryItem 1
        }
    let model = {
        Config = config
        DefaultPath = Ok config.DefaultPath
        CommandBindings = MainCommand.commandList |> List.map keyBinding
        KeyBindingIndex = 0
    }
    let textEdit = TextEdit.Dialog(window)
    let keyBinder = KeyBinder.Dialog(window)
    Framework.start binder events (dispatcher textEdit keyBinder) window model

let showDialog parent config =
    SettingsWindow(Owner = parent)
        .ShowDialog(start config)
        .Config
