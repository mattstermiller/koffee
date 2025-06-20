module Koffee.KeyBinder

open System.Windows.Input
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp
open UIHelpers
open Koffee
open KoffeeUI

type State =
    | BindKey
    | Review

type Model = {
    State: State
    KeyCombo: KeyCombo
}

type Events =
    | KeyPress // of ?
    | Cancel

let private binder (command: MainCommand) (window: KeyBinderWindow) (model: Model) =
    window.Header.Content <- "Key Binding for " + command.Name

    [
        Bind.model(<@ model.KeyCombo @>).toFunc(fun keyCombo ->
            window.KeyBinding.Content <- KeyBindingLogic.keyComboDescription keyCombo
        )
        Bind.model(<@ model.State @>).toFunc(fun state ->
            let promptText =
                match state with
                | BindKey ->
                    "Press a key combination or Escape to cancel..."
                | Review ->
                    [
                        // if chords > 0 then
                        "A to add a key to the sequence"
                        "Enter to accept, or Escape to cancel"
                    ]
                    |> String.concat ", "
                    |> sprintf "Press %s..."
            window.Prompt.Text <- promptText
        )
    ]

module Obs = Observable

let private events (window: KeyBinderWindow) = []
    // window.PreviewKeyDown

let private dispatcher evt =
    match evt with
    | _ -> Sync id

let private start commandToBind window =
    let model = {
        State = BindKey
        KeyCombo = []
    }
    Framework.start (binder commandToBind) events dispatcher window model

let showDialog parent commandToBind =
    KeyBinderWindow(Owner = parent).ShowDialog(start commandToBind).KeyCombo
