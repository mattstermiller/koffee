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
    KeyCombo1: KeyCombo option
    KeyCombo2: KeyCombo option
}
with
    static member normalize (binding: CommandBinding) =
        if binding.KeyCombo1 = binding.KeyCombo2 then
            { binding with KeyCombo2 = None }
        else if binding.KeyCombo1.IsNone && binding.KeyCombo2.IsSome then
            { binding with KeyCombo1 = binding.KeyCombo2; KeyCombo2 = None }
        else
            binding

    static member keyBindings (binding: CommandBinding) =
        [binding.KeyCombo1; binding.KeyCombo2]
        |> List.choose (Option.map (fun keyCombo -> {
            Command = binding.Command
            KeyCombo = keyCombo
        }))

    static member conflictsWith (keyCombo: KeyCombo) (binding: CommandBinding) =
        [binding.KeyCombo1; binding.KeyCombo2]
        |> List.exists (Option.exists (KeyCombo.startsWith keyCombo))

    static member removeKeyCombo (keyCombo: KeyCombo) (binding: CommandBinding) =
        if binding.KeyCombo1 |> Option.exists (KeyCombo.startsWith keyCombo) then
            { binding with KeyCombo1 = binding.KeyCombo2; KeyCombo2 = None }
        else if binding.KeyCombo2 |> Option.exists (KeyCombo.startsWith keyCombo) then
            { binding with KeyCombo2 = None }
        else
            binding

type State =
    | ListenForKey
    | WaitForCommand

type Model = {
    Command: MainCommand
    ExistingBindings: CommandBinding list
    State: State
    KeyCombo: KeyCombo
    KeyComboConflictsWithCommands: MainCommand list
    Accepted: bool
}
with
    static member create command keyCombo commandBindings = {
        Command = command
        ExistingBindings = commandBindings
        State = WaitForCommand
        KeyCombo = keyCombo
        KeyComboConflictsWithCommands = []
        Accepted = false
    }

type Events =
    | KeyPress of (ModifierKeys * Key) * KeyPressHandler
    | Cancel

let private binder (window: KeyBinderWindow) (model: Model) =
    window.CommandName.Text <- model.Command.Name

    [
        Bind.model(<@ model.KeyCombo @>).toFunc(fun keyCombo ->
            window.KeyBinding.Text <-
                if keyCombo.IsEmpty
                then "<Not Bound>"
                else sprintf "Keys: %s" (KeyBindingLogic.keyComboDescription keyCombo)
        )
        Bind.modelMulti(<@ model.State, model.KeyCombo @>).toFunc(fun (state, keyCombo) ->
            let promptText =
                match state with
                | ListenForKey ->
                    "Press a key combination or Escape to cancel..."
                | WaitForCommand ->
                    [
                        if keyCombo.Length < maxChords then
                            "A to add a key to the sequence"
                        if keyCombo.Length > 0 then
                            "C to replace the keys"
                            "Backspace to clear"
                        "Enter to accept"
                        "Escape to cancel"
                    ]
                    |> Seq.map (sprintf "Press %s")
                    |> String.concat "\n"
            window.Prompt.Text <- promptText
        )
        Bind.model(<@ model.KeyComboConflictsWithCommands @>).toFunc(fun otherCommands ->
            window.Conflict.Text <-
                if otherCommands.IsEmpty then
                    ""
                else
                    let names =
                        otherCommands
                        |> List.truncate maxListedConflicts
                        |> List.map (fun cmd -> cmd.Name)
                    let more =
                        if names.Length > maxListedConflicts
                        then sprintf "and %i more" (names.Length - maxListedConflicts)
                        else ""
                    sprintf "Accepting will clear existing binding%s for: %s%s"
                        (Format.pluralS names) (names |> String.concat ", ") more
            window.Conflict.IsCollapsed <- otherCommands.IsEmpty
        )
    ]

module Obs = Observable

let private events (window: KeyBinderWindow) = [
    window.PreviewKeyDown |> Obs.filter isNotModifier |> Obs.map (fun evt ->
        match evt.Chord with
        | (ModifierKeys.None, Key.Escape) -> Cancel
        | chord -> KeyPress (chord, evt.Handler)
    )
]

type EventHandler(closeWindow: unit -> unit) =
    let keyPress (chord: ModifierKeys * Key) (keyPressHandler: KeyPressHandler) (model: Model) =
        let modelOption =
            match model.State with
            | ListenForKey ->
                let keyCombo = model.KeyCombo @ [chord]
                let conflictsWithBindings =
                    model.ExistingBindings
                    |> Seq.filter (CommandBinding.conflictsWith keyCombo)
                    |> Seq.map (fun cb -> cb.Command)
                    |> Seq.except [model.Command]
                    |> Seq.toList
                Some (
                    { model with
                        KeyCombo = keyCombo
                        KeyComboConflictsWithCommands = conflictsWithBindings
                        State = WaitForCommand
                    }
                )
            | WaitForCommand ->
                match chord with
                | (ModifierKeys.None, Key.A) when model.KeyCombo.Length < maxChords ->
                    Some { model with State = ListenForKey }
                | (ModifierKeys.None, Key.C) ->
                    Some { model with KeyCombo = []; KeyComboConflictsWithCommands = []; State = ListenForKey }
                | (ModifierKeys.None, Key.Back) ->
                    Some { model with KeyCombo = []; KeyComboConflictsWithCommands = [] }
                | (ModifierKeys.None, Key.Enter) ->
                    closeWindow()
                    Some { model with Accepted = true }
                | _ ->
                    None
        match modelOption with
        | Some newModel ->
            keyPressHandler.Handle()
            newModel
        | None ->
            model

    let cancel (model: Model) =
        match model.State with
        | ListenForKey ->
            { model with State = WaitForCommand }
        | WaitForCommand ->
            closeWindow()
            model

    member _.Handle (evt: Events) =
        match evt with
        | KeyPress (chord, keyHandler) -> Sync (keyPress chord keyHandler)
        | Cancel -> Sync cancel

let private start model (window: KeyBinderWindow) =
    let handler = EventHandler(fun () -> window.Close())
    Framework.start binder events handler.Handle window model

type Dialog(parent: Window) =
    member _.Open commandToBind (currentKeyCombo: KeyCombo option) commandBindings =
        let startModel = Model.create commandToBind (currentKeyCombo |? []) commandBindings
        let model = KeyBinderWindow(Owner = parent).ShowDialog(start startModel)
        if model.Accepted
        then Some model.KeyCombo
        else None
