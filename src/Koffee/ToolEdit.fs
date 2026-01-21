module Koffee.ToolEdit

open System
open System.Windows
open System.Windows.Input
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp
open UIHelpers
open KoffeeUI

module Errors =
    let cannotBeEmpty = "Cannot be empty"
    let nameAlreadyUsed = "Name already used by another tool"

type Model = {
    ToolName: Result<string, string>
    Exe: Result<string, string>
    Arguments: Result<string, string>
    HideWindow: bool
    OriginalTool: Tool option
    SavedTool: Tool option
}
with
    member this.ToolResult =
        let prefixError prefix =
            Result.mapError (sprintf "%s is invalid: %s" prefix)
        result {
            let! name = this.ToolName |> prefixError "Name"
            let! exe = this.Exe |> prefixError "Command/Exe"
            let! args = this.Arguments |> prefixError "Arguments"
            return {
                ToolName = name.Trim()
                Exe = exe
                Arguments = args
                HideWindow = this.HideWindow
            }
        }

    static member create (toolToEdit: Tool option) =
        match toolToEdit with
        | Some tool ->
            {
                ToolName = Ok tool.ToolName
                Exe = Ok tool.Exe
                Arguments = Ok tool.Arguments
                HideWindow = tool.HideWindow
                OriginalTool = toolToEdit
                SavedTool = None
            }
        | None ->
            {
                ToolName = Error Errors.cannotBeEmpty
                Exe = Error Errors.cannotBeEmpty
                Arguments = Ok String.Empty
                HideWindow = false
                OriginalTool = toolToEdit
                SavedTool = None
            }

type Events =
    | Save
    | Cancel
    | Closing of cancel: (unit -> unit)

let private binder existingToolNames (window: ToolEditWindow) (model: Model) =
    let existingToolNames = existingToolNames |> Set.ofSeq
    let validateNotEmpty (s: String) =
        if s |> String.isNotWhiteSpace
        then Ok s
        else Error Errors.cannotBeEmpty
    let validateName (name: String) =
        name
        |> validateNotEmpty
        |> Result.errorIf (String.trim >> existingToolNames.Contains) Errors.nameAlreadyUsed

    window.ArgumentsHelp.ToolTip <-
        String.concat "\n" (seq {
            "Variables may be used:"
            yield! ToolVariable.examples |> Seq.map (sprintf "- %s")
            ""
            "Surround in brackets [] to make optional."
            "Override item delimiter by adding colon : and desired separator."
            "Add fallback by adding question mark ? and another variable name."
            ""
            "Example:"
            "{git_root?location} [--select {selected_files:,}] --save-to {env:USERPROFILE}"
        })

    window.PreviewKeyDown.Add (onKey Key.Escape window.Close)

    window.Loaded.Add(fun _ ->
        [window.ToolName; window.Exe; window.Arguments] |> List.iter (fun textBox -> textBox.SelectAll())
        window.ToolName.Focus() |> ignore
    )

    [
        Bind.view(<@ window.ToolName.Text @>).toModelResult(<@ model.ToolName @>, validateName, id, OnChange)
        Bind.view(<@ window.Exe.Text @>).toModelResult(<@ model.Exe @>, validateNotEmpty, id, OnChange)
        Bind.view(<@ window.Arguments.Text @>).toModelResult(<@ model.Arguments @>, Ok, id, OnChange)
        Bind.view(<@ window.HideWindow.IsChecked @>).toModel(<@ model.HideWindow @>)

        Bind.model(<@ model.ToolResult @>).toFunc(fun tool -> window.SaveButton.IsEnabled <- tool.IsOk)
    ]

module Obs = Observable

let private events (window: ToolEditWindow) = [
    window.SaveButton.Click |> Obs.mapTo Save
    window.CancelButton.Click |> Obs.mapTo Cancel
    window.Closing |> Obs.map (fun e -> Closing (fun () -> e.Cancel <- true))
    window.PreviewKeyDown |> Obs.filter (fun evt -> evt.Key = Key.Enter) |> Obs.map (fun evt ->
        evt.Handled <- true
        Save
    )
]

let private save closeWindow (model: Model) =
    match model.ToolResult with
    | Ok tool ->
        closeWindow()
        { model with SavedTool = Some tool }
    | Error error ->
        MessageBox.Show(error, "Validation Error") |> ignore
        model

let private closing cancel (model: Model) =
    if model.SavedTool = None && model <> Model.create model.OriginalTool then
        let discard = openConfirmationDialog "Discard Changes?" "You have unsaved changes. Do you want to discard them?"
        if not discard then
            cancel()
    model

let private dispatcher closeWindow evt =
    match evt with
    | Save -> Sync (save closeWindow)
    | Cancel -> Sync (fun m -> closeWindow(); m)
    | Closing cancel -> Sync (closing cancel)

let private start existingToolNames toolToEdit (window: ToolEditWindow) =
    let model = Model.create toolToEdit
    Framework.start (binder existingToolNames) events (dispatcher window.Close) window model

type Dialog(parent: Window) =
    member _.Open existingToolNames toolToEdit =
        let model = ToolEditWindow(Owner = parent).ShowDialog(start existingToolNames toolToEdit)
        model.SavedTool
