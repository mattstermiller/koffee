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
    CommandBindingIndex: int
    BindingsKeysPressed: KeyCombo
    PageSize: int
}
with
    member this.SelectedCommandBinding = this.CommandBindings.[this.CommandBindingIndex]

    member this.withCommandBindings commandBindings =
        { this with
            CommandBindings = commandBindings
            Model.Config.KeyBindings = commandBindings |> List.collect CommandBinding.keyBindings
        }

    static member create config commandBindings = {
        Config = config
        DefaultPath = Ok config.DefaultPath
        CommandBindings = commandBindings
        CommandBindingIndex = 0
        BindingsKeysPressed = []
        PageSize = 16
    }

type Events =
    | StartPathChanged of StartPath
    | DefaultPathChanged
    | PathFormatChanged of PathFormat
    | EditSearchExclusions
    | CommandsKeyPress of KeyChord * KeyPressHandler
    | EditCommandBindings
    | ResetCommandBindings
    | ResetAllKeyBindings
    | PageSizeChanged of int

let private binder (window: SettingsWindow) model =
    window.PreviewKeyDown.Add (onKey Key.Escape window.Close)
    window.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W window.Close)

    window.Commands.AddColumn(<@ fun cb -> cb.Command @>, "Command", conversion = (fun cmd -> cmd.Name), widthWeight = 3.0)
    window.Commands.AddColumn(<@ fun cb -> cb.KeyCombo1 @>, "Key Combo 1", conversion = KeyCombo.displayString)
    window.Commands.AddColumn(<@ fun cb -> cb.KeyCombo2 @>, "Key Combo 2", conversion = KeyCombo.displayString)

    // fix tabbing into and out of grid to not focus cells
    let prevElement = window.EditSearchExclusions
    let nextElement = window.ResetAllKeyBindings
    prevElement.PreviewKeyDown.Add (onKey Key.Tab window.Commands.Focus)
    window.Commands.PreviewKeyDown.Add (onKey Key.Tab nextElement.Focus)
    nextElement.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Shift Key.Tab window.Commands.Focus)
    window.Commands.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Shift Key.Tab prevElement.Focus)

    window.Commands.KeepSelectedItemInView()

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
        Bind.view(<@ window.ShowFullPathInTitleBar.IsChecked @>).toModel(<@ model.Config.ShowFullPathInTitle @>)
        Bind.view(<@ window.RefreshOnActivate.IsChecked @>).toModel(<@ model.Config.RefreshOnActivate @>)

        Bind.modelMulti(<@ model.CommandBindings, model.CommandBindingIndex @>).toFunc(fun (bindings, index) ->
            if not (obj.ReferenceEquals(window.Commands.ItemsSource, bindings)) then
                if window.Commands.ItemsSource <> null then
                    window.Commands.Focus() |> ignore // focus grid when updating but not on initial load
                window.Commands.ItemsSource <- bindings
            window.Commands.SelectedIndex <- index
        )
        Bind.view(<@ window.Commands.SelectedIndex @>).toModelOneWay(<@ model.CommandBindingIndex @>)
    ]

module Obs = Observable

let private events (window: SettingsWindow) = [
    window.StartPathPrevious.Checked |> Obs.mapTo (StartPathChanged RestorePrevious)
    window.StartPathDefault.Checked |> Obs.mapTo (StartPathChanged DefaultPath)
    window.DefaultPath.LostFocus |> Obs.mapTo DefaultPathChanged
    window.EditSearchExclusions.Click |> Obs.mapTo EditSearchExclusions
    window.PathFormatWindows.Checked |> Obs.mapTo (PathFormatChanged Windows)
    window.PathFormatUnix.Checked |> Obs.mapTo (PathFormatChanged Unix)

    window.Commands.PreviewKeyDown |> Obs.filter isNotModifier |> Obs.map (fun evt ->
        CommandsKeyPress (evt.Chord, evt.Handler)
    )
    window.Commands.MouseDoubleClick |> Obs.mapTo EditCommandBindings
    window.Commands.SizeChanged |> Obs.throttle 0.5 |> Obs.onCurrent |> Obs.choose (fun _ ->
        window.Commands.VisibleRowCount |> Option.map PageSizeChanged
    )
    window.ResetAllKeyBindings.Click |> Obs.mapTo ResetAllKeyBindings
    window.ResetCommandBindings.Click |> Obs.mapTo ResetCommandBindings
    window.EditCommandBindings.Click |> Obs.mapTo EditCommandBindings
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

let private editCommandBinding (keyBinder: KeyBinder.Dialog) (model: Model) =
    match keyBinder.Open model.SelectedCommandBinding model.CommandBindings with
    | Some newBinding ->
        model.CommandBindings |> List.mapi (fun i binding ->
            if i = model.CommandBindingIndex
            then newBinding
            else binding |> CommandBinding.removeConflictingKeyCombos newBinding
        )
        |> model.withCommandBindings
    | None ->
        model

let private getCommandBinding keyBindings command =
    let bindings = KeyBinding.getKeyCombos keyBindings command
    {
        Command = command
        KeyCombo1 = bindings |> List.tryItem 0 |? []
        KeyCombo2 = bindings |> List.tryItem 1 |? []
    }

let private getCommandBindings keyBindings =
    MainCommand.commandList |> List.map (getCommandBinding keyBindings)

let private openConfirmationDialog title message =
    MessageBox.Show(message, title, MessageBoxButton.YesNo) = MessageBoxResult.Yes

let private resetCommandBinding (model: Model) =
    let selectedCommand = model.SelectedCommandBinding.Command
    let defaultBinding = getCommandBinding MainBindings.Default selectedCommand
    let conflicts =
        model.CommandBindings
        |> Seq.filter (fun binding -> binding.Command <> selectedCommand)
        |> Seq.filter (CommandBinding.conflictsWithBinding defaultBinding)
        |> Seq.map (fun binding -> binding.Command.Name)
        |> String.concat ", "
    let message =
        sprintf "Are you sure you want to reset the keybindings for %s to its default?" selectedCommand.Name
        + (conflicts |> String.ifNotEmpty (fun s -> "\n\nWARNING: The following commands will have bindings cleared: " + s))

    let confirmed = openConfirmationDialog "Reset selected command keybindings?" message
    if confirmed then
        model.CommandBindings |> List.mapi (fun i binding ->
            if i = model.CommandBindingIndex
            then defaultBinding
            else binding |> CommandBinding.removeConflictingKeyCombos defaultBinding
        )
        |> model.withCommandBindings
    else
        model

let private resetAllKeyBindings (model: Model) =
    let confirmed = openConfirmationDialog "Reset all keybindings?" "Are you sure you want to reset ALL keybindings to defaults?"
    if confirmed then
        { model with
            CommandBindings = MainBindings.Default |> getCommandBindings
            Config = { model.Config with KeyBindings = MainBindings.Default }
        }
    else
        model

let private commandsKeyPress keyBinder chord handleKey (model: Model) =
    let keyCombo = List.append model.BindingsKeysPressed [chord]
    match KeyBinding.getMatch model.Config.KeyBindings keyCombo with
    | Match evt ->
        handleKey ()
        let model = { model with BindingsKeysPressed = [] }
        let withCursor cursor =
            { model with CommandBindingIndex = cursor }
        let withCursorRel relValue =
            withCursor (model.CommandBindingIndex + relValue |> clamp 0 (model.CommandBindings.Length - 1))
        match evt with
        | Cursor CursorUp -> withCursorRel -1
        | Cursor CursorDown -> withCursorRel 1
        | Cursor CursorUpHalfPage -> withCursorRel -(model.PageSize/2)
        | Cursor CursorDownHalfPage -> withCursorRel (model.PageSize/2)
        | Cursor CursorToFirst -> withCursor 0
        | Cursor CursorToLast -> withCursor (model.CommandBindings.Length - 1)
        | Navigation OpenCursorItem
        | Navigation OpenSelected -> editCommandBinding keyBinder model
        | _ -> model
    | PartialMatch ->
        handleKey ()
        { model with BindingsKeysPressed = keyCombo }
    | NoMatch ->
        { model with BindingsKeysPressed = [] }

let private dispatcher textEdit keyBinder evt =
    match evt with
    | StartPathChanged value -> Sync (updateConfig (fun c -> { c with StartPath = value }))
    | DefaultPathChanged -> Sync defaultPathChanged
    | PathFormatChanged value -> Sync (updateConfig (fun c -> { c with PathFormat = value}))
    | EditSearchExclusions -> Sync (editSearchExclusions textEdit)
    | CommandsKeyPress (chord, handler) -> Sync (commandsKeyPress keyBinder chord handler.Handle)
    | EditCommandBindings -> Sync (editCommandBinding keyBinder)
    | ResetCommandBindings -> Sync resetCommandBinding
    | ResetAllKeyBindings -> Sync resetAllKeyBindings
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
