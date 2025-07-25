module Koffee.Settings

open System
open System.Windows
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
    BindingsKeysPressed: KeyCombo
    PageSize: int
}
with
    static member create config commandBindings = {
        Config = config
        DefaultPath = Ok config.DefaultPath
        CommandBindings = commandBindings
        KeyBindingIndex = 0
        BindingsKeysPressed = []
        PageSize = 16
    }

type Events =
    | StartPathChanged of StartPath
    | DefaultPathChanged
    | PathFormatChanged of PathFormat
    | EditSearchExclusions
    | BindingsKeyPress of (ModifierKeys * Key) * KeyPressHandler
    | EditKeyBindings
    | ResetKeyBindings
    | PageSizeChanged of int

let private binder (window: SettingsWindow) model =
    window.PreviewKeyDown.Add (onKey Key.Escape window.Close)
    window.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W window.Close)

    window.KeyBindings.AddColumn(<@ fun cb -> cb.Command @>, "Command", conversion = (fun cmd -> cmd.Name), widthWeight = 3.0)
    window.KeyBindings.AddColumn(<@ fun cb -> cb.KeyCombo1 @>, "Key Combo 1", conversion = KeyBindingLogic.keyComboDescription)
    window.KeyBindings.AddColumn(<@ fun cb -> cb.KeyCombo2 @>, "Key Combo 2", conversion = KeyBindingLogic.keyComboDescription)

    // fix tabbing into and out of grid to not focus cells
    let prevElement = window.EditSearchExclusions
    let nextElement = window.ResetKeyBindings
    prevElement.PreviewKeyDown.Add (onKey Key.Tab window.KeyBindings.Focus)
    window.KeyBindings.PreviewKeyDown.Add (onKey Key.Tab nextElement.Focus)
    nextElement.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Shift Key.Tab window.KeyBindings.Focus)
    window.KeyBindings.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Shift Key.Tab prevElement.Focus)

    window.KeyBindings.KeepSelectedItemInView()

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

        Bind.modelMulti(<@ model.CommandBindings, model.KeyBindingIndex @>).toFunc(fun (bindings, index) ->
            if not (obj.ReferenceEquals(window.KeyBindings.ItemsSource, bindings)) then
                if window.KeyBindings.ItemsSource <> null then
                    window.KeyBindings.Focus() |> ignore // focus grid when updating but not on initial load
                window.KeyBindings.ItemsSource <- bindings
            window.KeyBindings.SelectedIndex <- index
        )
        Bind.view(<@ window.KeyBindings.SelectedIndex @>).toModelOneWay(<@ model.KeyBindingIndex @>)
    ]

module Obs = Observable

let private events (window: SettingsWindow) = [
    window.StartPathPrevious.Checked |> Obs.mapTo (StartPathChanged RestorePrevious)
    window.StartPathDefault.Checked |> Obs.mapTo (StartPathChanged DefaultPath)
    window.DefaultPath.LostFocus |> Obs.mapTo DefaultPathChanged
    window.EditSearchExclusions.Click |> Obs.mapTo EditSearchExclusions
    window.PathFormatWindows.Checked |> Obs.mapTo (PathFormatChanged Windows)
    window.PathFormatUnix.Checked |> Obs.mapTo (PathFormatChanged Unix)

    window.KeyBindings.PreviewKeyDown |> Obs.filter isNotModifier |> Obs.map (fun evt ->
        BindingsKeyPress (evt.Chord, evt.Handler)
    )
    window.KeyBindings.MouseDoubleClick |> Obs.mapTo EditKeyBindings
    window.KeyBindings.SizeChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.choose (fun _ ->
        window.KeyBindings.VisibleRowCount |> Option.map PageSizeChanged
    )
    window.EditKeyBindings.Click |> Obs.mapTo EditKeyBindings
    window.ResetKeyBindings.Click |> Obs.mapTo ResetKeyBindings
]

let private updateConfig f (model: Model) =
    { model with Config = f model.Config }

let private defaultPathChanged (model: Model) =
    match model.DefaultPath with
    | Ok path -> model |> updateConfig (fun c -> { c with DefaultPath = path })
    | Error _ -> model

let private editSearchExclusions (textEdit: TextEdit.Dialog) (model: Model) =
    let searchExclusions =
        textEdit.Open
            "Recursive Search Exclusions"
            ("This list of folder names are excluded from recursive searches.\n" +
                "Disable exclusions by adding \"/\" in front of them.")
            (model.Config.SearchExclusions @ [""] |> String.concat "\n")
        |> String.split '\n' |> Array.toList |> List.map String.trim
    model |> updateConfig (fun c -> { c with SearchExclusions = searchExclusions })

let private editKeyBinding (keyBinder: KeyBinder.Dialog) (model: Model) =
    let bindingToEdit = model.CommandBindings |> List.item model.KeyBindingIndex
    match keyBinder.Open bindingToEdit model.CommandBindings with
    | Some newBinding ->
        let commandBindings =
            model.CommandBindings |> List.mapi (fun i binding ->
                if i = model.KeyBindingIndex
                then newBinding
                else binding |> CommandBinding.removeConflictingKeyCombos newBinding
            )
        { model with
            CommandBindings = commandBindings
            Config = { model.Config with KeyBindings = commandBindings |> List.collect CommandBinding.keyBindings }
        }
    | None ->
        model

let private getCommandBindings keyBindings =
    MainCommand.commandList |> List.map (fun command ->
        let bindings = KeyBindingLogic.getKeyCombos keyBindings command
        {
            Command = command
            KeyCombo1 = bindings |> List.tryItem 0 |? []
            KeyCombo2 = bindings |> List.tryItem 1 |? []
        }
    )

let private resetKeyBinding (model: Model) =
    let result = MessageBox.Show(
        "Are you sure you want to reset all keybindings to defaults?",
        "Reset all keybindings?",
        MessageBoxButton.YesNo
    )
    if result = MessageBoxResult.Yes then
        { model with
            CommandBindings = KeyBinding.Default |> getCommandBindings
            Config = { model.Config with KeyBindings = KeyBinding.Default }
        }
    else
        model

let private bindingsKeyPress keyBinder chord handleKey (model: Model) =
    let keyCombo = List.append model.BindingsKeysPressed [chord]
    // TODO: get match for command type (Input or not)
    match KeyBindingLogic.getMatch model.Config.KeyBindings keyCombo with
    | KeyBindingLogic.Match evt ->
        handleKey ()
        let model = { model with BindingsKeysPressed = [] }
        let withCursor cursor =
            { model with KeyBindingIndex = cursor }
        let withCursorRel relValue =
            withCursor (model.KeyBindingIndex + relValue |> clamp 0 (model.CommandBindings.Length - 1))
        match evt with
        | Cursor CursorUp -> withCursorRel -1
        | Cursor CursorDown -> withCursorRel 1
        | Cursor CursorUpHalfPage -> withCursorRel -(model.PageSize/2)
        | Cursor CursorDownHalfPage -> withCursorRel (model.PageSize/2)
        | Cursor CursorToFirst -> withCursor 0
        | Cursor CursorToLast -> withCursor (model.CommandBindings.Length - 1)
        | Navigation OpenCursorItem
        | Navigation OpenSelected -> editKeyBinding keyBinder model
        | _ -> model
    | KeyBindingLogic.PartialMatch ->
        handleKey ()
        { model with BindingsKeysPressed = keyCombo }
    | KeyBindingLogic.NoMatch ->
        { model with BindingsKeysPressed = [] }

let private dispatcher textEdit keyBinder evt =
    match evt with
    | StartPathChanged value -> Sync (updateConfig (fun c -> { c with StartPath = value }))
    | DefaultPathChanged -> Sync defaultPathChanged
    | PathFormatChanged value -> Sync (updateConfig (fun c -> { c with PathFormat = value}))
    | EditSearchExclusions -> Sync (editSearchExclusions textEdit)
    | BindingsKeyPress (chord, handler) -> Sync (bindingsKeyPress keyBinder chord handler.Handle)
    | EditKeyBindings -> Sync (editKeyBinding keyBinder)
    | ResetKeyBindings -> Sync resetKeyBinding
    | PageSizeChanged pageSize -> Sync (fun m -> { m with PageSize = pageSize })

let private start (config: Config) window =
    let model = Model.create config (getCommandBindings config.KeyBindings)
    let textEdit = TextEdit.Dialog(window)
    let keyBinder = KeyBinder.Dialog(window)
    Framework.start binder events (dispatcher textEdit keyBinder) window model

let showDialog parent config =
    SettingsWindow(Owner = parent)
        .ShowDialog(start config)
        .Config
