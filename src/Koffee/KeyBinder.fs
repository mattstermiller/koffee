module Koffee.KeyBinder

open System.Windows
open System.Windows.Input
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp
open UIHelpers
open Koffee
open KoffeeUI

let private maxChords = 2
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

    static member conflictsWithBinding (other: CommandBinding) (binding: CommandBinding) =
        CommandBinding.conflictsWith other.KeyCombo1 binding ||
        CommandBinding.conflictsWith other.KeyCombo2 binding

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
    | ChangeCombo
    | ClearChords
    | Undo
    | Close
with
    member this.Description =
        match this with
        | SelectCombo Combo1 -> "Select key combo 1"
        | SelectCombo Combo2 -> "Select key combo 2"
        | ChangeCombo -> "Change the combo"
        | ClearChords -> "Clear the combo"
        | Undo -> "Undo the last change"
        | Close -> "Save and close"

    static member getAvailableCommands hasCurrentKeyCombo hasUndo = [
        SelectCombo Combo1
        SelectCombo Combo2
        ChangeCombo
        if hasCurrentKeyCombo then
            ClearChords
        if hasUndo then
            Undo
        Close
    ]

    static member fromMainCommand (mainCommand: MainCommand) =
        match mainCommand with
        | Cursor CursorUp -> Some (SelectCombo Combo1)
        | Cursor CursorDown -> Some (SelectCombo Combo2)
        | Navigation OpenCursorItem -> Some ChangeCombo
        | Navigation OpenSelected -> Some ChangeCombo
        | ItemAction Trash -> Some ClearChords
        | ItemAction ItemActionCommand.Undo -> Some Undo
        | _ -> None

module KeyBinderBinding =
    let private noMod = ModifierKeys.None

    let private staticBindings = List.map KeyBinding.ofTuple [
        ([noMod, Key.Escape], Close)
    ]

    let private fallbackBindings = List.map KeyBinding.ofTuple [
        ([noMod, Key.Up], SelectCombo Combo1)
        ([noMod, Key.Down], SelectCombo Combo2)
        ([noMod, Key.Enter], ChangeCombo)
        ([noMod, Key.Delete], ClearChords)
        ([noMod, Key.U], Undo)
    ]

    let getBindings mainBindings =
        let mainCommandsTranslated =
            mainBindings
            |> List.collect CommandBinding.keyBindings
            |> List.choose (fun keyBinding ->
                keyBinding.Command
                |> Command.fromMainCommand
                |> Option.map (fun cmd -> (keyBinding.KeyCombo, cmd) |> KeyBinding.ofTuple)
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
            KeyBinding.getKeyCombos bindings cmd
            |> Seq.map KeyCombo.displayString
            |> String.concat " or "
        sprintf "%s - %s" cmd.Description combos

type State =
    | WaitForCommand of KeyCombo
    | ListenForKey

type Model = {
    ExistingBindings: CommandBinding list
    Binding: CommandBinding
    BindingUndoStack: CommandBinding list
    KeyBinderBindings: KeyBinding<Command> list
    ComboSelection: ComboSelection
    Combo1Conflicts: MainCommand list
    Combo1CannotBeUsedInInput: bool
    Combo2Conflicts: MainCommand list
    Combo2CannotBeUsedInInput: bool
    State: State
}
with
    member this.SelectedCombo =
        match this.ComboSelection with
        | Combo1 -> this.Binding.KeyCombo1
        | Combo2 -> this.Binding.KeyCombo2

    static member create editBinding commandBindings = {
        ExistingBindings = commandBindings
        Binding = editBinding
        BindingUndoStack = []
        KeyBinderBindings = KeyBinderBinding.getBindings commandBindings
        ComboSelection = Combo1
        Combo1Conflicts = []
        Combo1CannotBeUsedInInput = false
        Combo2Conflicts = []
        Combo2CannotBeUsedInInput = false
        State = WaitForCommand []
    }

    static member withComputedConflicts (comboSelection: ComboSelection) (model: Model) =
        let combo =
            match comboSelection with
            | Combo1 -> model.Binding.KeyCombo1
            | Combo2 -> model.Binding.KeyCombo2
        let conflictsWithBindings =
            model.ExistingBindings
            |> Seq.filter (CommandBinding.conflictsWith combo)
            |> Seq.map (fun cb -> cb.Command)
            |> Seq.except [model.Binding.Command]
            |> Seq.toList
        let comboCannotBeUsedInInput =
            model.Binding.Command = Cursor FindNext && not combo.IsEmpty && not (combo |> KeyCombo.isUsableInInputBox)
        match comboSelection with
        | Combo1 ->
            { model with
                Combo1Conflicts = conflictsWithBindings
                Combo1CannotBeUsedInInput = comboCannotBeUsedInInput
            }
        | Combo2 ->
            { model with
                Combo2Conflicts = conflictsWithBindings
                Combo2CannotBeUsedInInput = comboCannotBeUsedInInput
            }

    static member clearSelectedCombo (model: Model) =
        match model.ComboSelection with
        | Combo1 ->
            { model with
                Binding.KeyCombo1 = []
                BindingUndoStack = model.Binding :: model.BindingUndoStack
                Combo1Conflicts = []
                Combo1CannotBeUsedInInput = false
            }
        | Combo2 ->
            { model with
                Binding.KeyCombo2 = []
                BindingUndoStack = model.Binding :: model.BindingUndoStack
                Combo2Conflicts = []
                Combo2CannotBeUsedInInput = false
            }

type Events =
    | KeyPress of KeyChord * KeyPressHandler
    | ComboFocused of ComboSelection

let private viewBinder (window: KeyBinderWindow) (model: Model) =
    window.CommandName.Text <- model.Binding.Command.Name

    let selectedStyle = window.FindResource "Selected" :?> Style

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
                sprintf "Closing will clear existing binding%s for: %s%s"
                    (Format.pluralS names) (names |> String.concat ", ") more
        ]
        |> String.concat "\n"

    [
        Bind.model(<@ model.Binding.KeyCombo1 @>).toFunc(KeyCombo.displayString >> window.KeyCombo1.set_Text)
        Bind.model(<@ model.Binding.KeyCombo2 @>).toFunc(KeyCombo.displayString >> window.KeyCombo2.set_Text)
        Bind.model(<@ model.ComboSelection @>).toFunc(fun selection ->
            let style1, style2 =
                match selection with
                | Combo1 -> (selectedStyle, null)
                | Combo2 -> (null, selectedStyle)
            window.KeyComboPanel1.Style <- style1
            window.KeyComboPanel2.Style <- style2
        )
        Bind.modelMulti(<@ model.State, model.SelectedCombo, model.BindingUndoStack @>)
            .toFunc(fun (state, keyCombo, undoStack) ->
                let promptText =
                    match state with
                    | WaitForCommand _ ->
                        Command.getAvailableCommands (not keyCombo.IsEmpty) (not undoStack.IsEmpty)
                        |> Seq.map (KeyBinderBinding.getPrompt model.KeyBinderBindings)
                        |> String.concat "\n"
                    | ListenForKey ->
                        "Press a key combination or Escape to finish..."
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
        | ChangeCombo ->
            { model with State = ListenForKey } |> Model.clearSelectedCombo
        | ClearChords ->
            model |> Model.clearSelectedCombo
        | Undo ->
            match model.BindingUndoStack with
            | undoBinding :: undoRest ->
                let changedCombo = if undoBinding.KeyCombo1 <> model.Binding.KeyCombo1 then Combo1 else Combo2
                { model with
                    Binding = undoBinding
                    BindingUndoStack = undoRest
                }
                |> Model.withComputedConflicts changedCombo
            | [] ->
                model
        | Close ->
            closeWindow()
            model

    let keyPress (chord: KeyChord) (keyPressHandler: KeyPressHandler) (model: Model) =
        match model.State with
        | WaitForCommand keyCombo ->
            let keyCombo = List.append keyCombo [chord]
            match KeyBinding.getMatch model.KeyBinderBindings keyCombo with
            | Match command ->
                keyPressHandler.Handle()
                { model with State = WaitForCommand [] } |> handleCommand command
            | PartialMatch ->
                keyPressHandler.Handle()
                { model with State = WaitForCommand keyCombo }
            | NoMatch ->
                { model with State = WaitForCommand [] }
        | ListenForKey ->
            keyPressHandler.Handle()
            match chord with
            | (ModifierKeys.None, DigitKey _) when model.SelectedCombo |> List.isEmpty ->
                // prevent binding first chord to digit since that is for repeat feature
                model
            | (ModifierKeys.None, Key.Escape) ->
                { model with State = WaitForCommand [] } |> Model.withComputedConflicts model.ComboSelection
            | _ ->
                let keyCombo =
                    if model.SelectedCombo.Length >= maxChords
                    then [chord]
                    else model.SelectedCombo @ [chord]
                match model.ComboSelection with
                | Combo1 -> { model with Binding.KeyCombo1 = keyCombo }
                | Combo2 -> { model with Binding.KeyCombo2 = keyCombo }

    member _.Handle (evt: Events) =
        match evt with
        | KeyPress (chord, keyHandler) -> Sync (keyPress chord keyHandler)
        | ComboFocused selection -> Sync (fun model -> { model with ComboSelection = selection })

let private start model (window: KeyBinderWindow) =
    let handler = EventHandler(fun () -> window.Close())
    Framework.start viewBinder events handler.Handle window model

type Dialog(parent: Window) =
    member _.Open (editBinding: CommandBinding) commandBindings =
        let startModel = Model.create editBinding commandBindings
        let model = KeyBinderWindow(Owner = parent).ShowDialog(start startModel)
        if model.Binding <> editBinding
        then Some model.Binding
        else None
