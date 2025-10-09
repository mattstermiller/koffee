module Koffee.KeyBinder

open System.Windows
open System.Windows.Input
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp
open UIHelpers
open Koffee
open KoffeeUI

let private maxChords = 3
let private maxListedConflicts = 3

type CommandBinding = {
    Command: MainCommand
    KeyCombo1: KeyCombo
    KeyCombo2: KeyCombo
}
with
    static member normalize (binding: CommandBinding) =
        if binding.KeyCombo1 = binding.KeyCombo2 then
            { binding with KeyCombo2 = [] }
        else if binding.KeyCombo1.IsEmpty && not binding.KeyCombo2.IsEmpty then
            { binding with KeyCombo1 = binding.KeyCombo2; KeyCombo2 = [] }
        else
            binding

    static member keyCombos (binding: CommandBinding) =
        [binding.KeyCombo1; binding.KeyCombo2]
        |> List.filter (not << List.isEmpty)

    static member keyBindings (binding: CommandBinding) =
        binding
        |> CommandBinding.keyCombos
        |> List.map (fun keyCombo -> {
            Command = binding.Command
            KeyCombo = keyCombo
        })

    static member conflictsWith (keyCombo: KeyCombo) (binding: CommandBinding) =
        if not keyCombo.IsEmpty then
            binding
            |> CommandBinding.keyCombos
            |> List.exists (KeyCombo.intersectsWith keyCombo)
        else
            false

    static member removeConflictingKeyCombos (newBinding: CommandBinding) (binding: CommandBinding) =
        if newBinding |> CommandBinding.conflictsWith binding.KeyCombo2 then
            { binding with KeyCombo2 = [] }
            |> CommandBinding.removeConflictingKeyCombos newBinding
        else if newBinding |> CommandBinding.conflictsWith binding.KeyCombo1 then
            { binding with KeyCombo1 = binding.KeyCombo2; KeyCombo2 = [] }
        else
            binding

type ComboSelection =
    | Combo1
    | Combo2

type Command =
    | SelectCombo of ComboSelection
    | AddChord
    | ReplaceChords
    | ClearChords
    | SaveBindings
    | Cancel
with
    member this.Description =
        match this with
        | SelectCombo Combo1 -> "Select key combo 1"
        | SelectCombo Combo2 -> "Select key combo 2"
        | AddChord -> "Add a key to the combo"
        | ReplaceChords -> "Replace the combo"
        | ClearChords -> "Clear the combo"
        | SaveBindings -> "Save command bindings"
        | Cancel -> "Cancel"

    static member getAvailableCommands currentKeyComboLength = [
        SelectCombo Combo1
        SelectCombo Combo2
        if currentKeyComboLength < maxChords then
            AddChord
        if currentKeyComboLength > 0 then
            ReplaceChords
            ClearChords
        SaveBindings
        Cancel
    ]

    static member fromMainCommand (mainCommand: MainCommand) =
        match mainCommand with
        | Cursor CursorUp -> Some (SelectCombo Combo1)
        | Cursor CursorDown -> Some (SelectCombo Combo2)
        | ItemAction (StartRename EndName) -> Some AddChord
        | ItemAction Recycle -> Some ClearChords
        | _ -> None

module KeyBinderBinding =
    let private noMod = ModifierKeys.None

    let private staticBindings = List.map KeyBinding<Command>.ofTuple [
        ([noMod, Key.Enter], SaveBindings)
        ([noMod, Key.Escape], Cancel)
    ]

    let private fallbackBindings = List.map KeyBinding<Command>.ofTuple [
        ([noMod, Key.Up], SelectCombo Combo1)
        ([noMod, Key.Down], SelectCombo Combo2)
        ([noMod, Key.A], AddChord)
        ([noMod, Key.C], ReplaceChords)
        ([noMod, Key.Delete], ClearChords)
    ]

    let getBindings mainBindings =
        let mainCommandsTranslated =
            mainBindings
            |> List.collect CommandBinding.keyBindings
            |> List.choose (fun keyBinding ->
                keyBinding.Command
                |> Command.fromMainCommand
                |> Option.map (fun cmd -> (keyBinding.KeyCombo, cmd) |> KeyBinding<Command>.ofTuple)
            )
        let bindings = staticBindings @ mainCommandsTranslated
        let boundCommands = bindings |> List.map (fun b -> b.Command)
        let fallbacksToAdd =
            fallbackBindings |> List.choose (fun fallback ->
                if not (boundCommands |> List.contains fallback.Command)
                then Some fallback
                else None
            )
        bindings @ fallbacksToAdd

    let getPrompt (bindings: KeyBinding<Command> list) cmd =
        let combos =
            KeyBindingLogic.getKeyCombos bindings cmd
            |> Seq.map KeyBindingLogic.keyComboDescription
            |> String.concat " or "
        sprintf "%s - %s" cmd.Description combos

type State =
    | WaitForCommand of KeyCombo
    | ListenForKey

type Model = {
    ExistingBindings: CommandBinding list
    Binding: CommandBinding
    KeyBinderBindings: KeyBinding<Command> list
    ComboSelection: ComboSelection
    Combo1Conflicts: MainCommand list
    Combo1CannotBeUsedInInput: bool
    Combo2Conflicts: MainCommand list
    Combo2CannotBeUsedInInput: bool
    State: State
    SaveBindings: bool
}
with
    member this.SelectedCombo =
        match this.ComboSelection with
        | Combo1 -> this.Binding.KeyCombo1
        | Combo2 -> this.Binding.KeyCombo2

    static member create editBinding commandBindings = {
        ExistingBindings = commandBindings
        Binding = editBinding
        KeyBinderBindings = KeyBinderBinding.getBindings commandBindings
        ComboSelection = Combo1
        Combo1Conflicts = []
        Combo1CannotBeUsedInInput = false
        Combo2Conflicts = []
        Combo2CannotBeUsedInInput = false
        State = WaitForCommand []
        SaveBindings = false
    }

    static member clearSelectedCombo (model: Model) =
        match model.ComboSelection with
        | Combo1 ->
            { model with
                Binding = { model.Binding with KeyCombo1 = [] }
                Combo1Conflicts = []
                Combo1CannotBeUsedInInput = false
            }
        | Combo2 ->
            { model with
                Binding = { model.Binding with KeyCombo2 = [] }
                Combo2Conflicts = []
                Combo2CannotBeUsedInInput = false
            }

type Events =
    | KeyPress of (ModifierKeys * Key) * KeyPressHandler
    | ComboFocused of ComboSelection

let private binder (window: KeyBinderWindow) (model: Model) =
    window.CommandName.Text <- model.Binding.Command.Name

    let selectedStyle = window.FindResource "Selected" :?> Style

    let keyComboText keyCombo =
        keyCombo |> KeyBindingLogic.keyComboDescription

    let conflictText (otherCommands: MainCommand list, comboCannotBeUsedInInput) =
        [
            if comboCannotBeUsedInInput then
                "Warning: Letter keys and multi-chord combos cannot be used when the Input box is open."
            if not otherCommands.IsEmpty then
                let names =
                    otherCommands
                    |> List.truncate maxListedConflicts
                    |> List.map (fun cmd -> cmd.Name)
                let more =
                    if names.Length > maxListedConflicts
                    then sprintf "and %i more" (names.Length - maxListedConflicts)
                    else ""
                sprintf "Saving will clear existing binding%s for: %s%s"
                    (Format.pluralS names) (names |> String.concat ", ") more
        ]
        |> String.concat "\n"

    [
        Bind.model(<@ model.Binding.KeyCombo1 @>).toFunc(keyComboText >> window.KeyCombo1.set_Text)
        Bind.model(<@ model.Binding.KeyCombo2 @>).toFunc(keyComboText >> window.KeyCombo2.set_Text)
        Bind.model(<@ model.ComboSelection @>).toFunc(fun selection ->
            let style1, style2 =
                match selection with
                | Combo1 -> (selectedStyle, null)
                | Combo2 -> (null, selectedStyle)
            window.KeyComboPanel1.Style <- style1
            window.KeyComboPanel2.Style <- style2
        )
        Bind.modelMulti(<@ model.State, model.SelectedCombo @>).toFunc(fun (state, keyCombo) ->
            let promptText =
                match state with
                | WaitForCommand _ ->
                    Command.getAvailableCommands keyCombo.Length
                    |> Seq.map (KeyBinderBinding.getPrompt model.KeyBinderBindings)
                    |> String.concat "\n"
                | ListenForKey ->
                    "Press a key combination or Escape to cancel..."
            window.Prompt.Text <- promptText
        )
        Bind.modelMulti(<@ model.Combo1Conflicts, model.Combo1CannotBeUsedInInput @>).toFunc(conflictText >> fun text ->
            window.Conflict1.Text <- text
            window.Conflict1.IsCollapsed <- text |> String.isEmpty
        )
        Bind.modelMulti(<@ model.Combo2Conflicts, model.Combo2CannotBeUsedInInput @>).toFunc(conflictText >> fun text ->
            window.Conflict2.Text <- text
            window.Conflict2.IsCollapsed <- text |> String.isEmpty
        )
    ]

module Obs = Observable

let private events (window: KeyBinderWindow) =
    let focusHandler selection (evt: RoutedEventArgs) =
        evt.Handled <- true
        ComboFocused selection

    [
        window.PreviewKeyDown |> Obs.filter isNotModifier |> Obs.map (fun evt -> KeyPress (evt.Chord, evt.Handler))
        window.KeyComboPanel1.PreviewMouseDown |> Obs.map (focusHandler Combo1)
        window.KeyComboPanel2.PreviewMouseDown |> Obs.map (focusHandler Combo2)
    ]

type EventHandler(closeWindow: unit -> unit) =
    let handleCommand command model =
        match command with
        | SelectCombo selection ->
            { model with ComboSelection = selection }
        | AddChord ->
            if model.SelectedCombo.Length < maxChords
            then { model with State = ListenForKey }
            else model
        | ReplaceChords ->
            { model with State = ListenForKey } |> Model.clearSelectedCombo
        | ClearChords ->
            model |> Model.clearSelectedCombo
        | SaveBindings ->
            closeWindow()
            { model with SaveBindings = true }
        | Cancel ->
            closeWindow()
            model

    let keyPress (chord: ModifierKeys * Key) (keyPressHandler: KeyPressHandler) (model: Model) =
        match model.State with
        | WaitForCommand keyCombo ->
            let keyCombo = List.append keyCombo [chord]
            match KeyBindingLogic.getMatch model.KeyBinderBindings keyCombo with
            | KeyBindingLogic.Match command ->
                keyPressHandler.Handle()
                { model with State = WaitForCommand [] } |> handleCommand command
            | KeyBindingLogic.PartialMatch ->
                keyPressHandler.Handle()
                { model with State = WaitForCommand keyCombo }
            | KeyBindingLogic.NoMatch ->
                { model with State = WaitForCommand [] }
        | ListenForKey ->
            keyPressHandler.Handle()
            match chord with
            | (ModifierKeys.None, Key.Escape) ->
                { model with State = WaitForCommand [] }
            | (ModifierKeys.None, KeyBindingLogic.DigitKey _) when model.SelectedCombo |> List.isEmpty ->
                // prevent binding first chord to digit since that is for repeat feature
                model
            | _ ->
                let keyCombo = model.SelectedCombo @ [chord]
                let conflictsWithBindings =
                    model.ExistingBindings
                    |> Seq.filter (CommandBinding.conflictsWith keyCombo)
                    |> Seq.map (fun cb -> cb.Command)
                    |> Seq.except [model.Binding.Command]
                    |> Seq.toList
                let comboCannotBeUsedInInput =
                    model.Binding.Command = Cursor FindNext && not (keyCombo |> KeyBindingLogic.isComboUsableInInputBox)
                match model.ComboSelection with
                | Combo1 ->
                    { model with
                        State = WaitForCommand []
                        Binding = { model.Binding with KeyCombo1 = keyCombo }
                        Combo1Conflicts = conflictsWithBindings
                        Combo1CannotBeUsedInInput = comboCannotBeUsedInInput
                    }
                | Combo2 ->
                    { model with
                        State = WaitForCommand []
                        Binding = { model.Binding with KeyCombo2 = keyCombo }
                        Combo2Conflicts = conflictsWithBindings
                        Combo2CannotBeUsedInInput = comboCannotBeUsedInInput
                    }

    member _.Handle (evt: Events) =
        match evt with
        | KeyPress (chord, keyHandler) -> Sync (keyPress chord keyHandler)
        | ComboFocused selection -> Sync (fun model -> { model with ComboSelection = selection })

let private start model (window: KeyBinderWindow) =
    let handler = EventHandler(fun () -> window.Close())
    Framework.start binder events handler.Handle window model

type Dialog(parent: Window) =
    member _.Open (editBinding: CommandBinding) commandBindings =
        let startModel = Model.create editBinding commandBindings
        let model = KeyBinderWindow(Owner = parent).ShowDialog(start startModel)
        if model.SaveBindings
        then Some model.Binding
        else None
