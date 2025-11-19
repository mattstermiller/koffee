module Koffee.SettingsTests

open NUnit.Framework
open FsUnitTyped

type Model = Settings.Model

[<Test>]
let ``resetAllKeyBindings only resets Tool bindings for default Tools`` () =
    let customTool =
        { Tool.DefaultTextEditor with
            ToolName = "Vim"
            Exe = "gvim"
        }
    let customToolBinding =
        {
            Command = customTool.GetCommand()
            KeyCombo = [ModifierKeys.Alt, Key.V]
        }
    let customTerminalBinding =
        {
            Command = Tool.DefaultTerminal.GetCommand()
            KeyCombo = [ModifierKeys.Alt, Key.T]
        }

    let bindings =
        MainBindings.Default
        // remove default tool bindings
        |> List.filter (fun kb -> kb.Command.ToolName.IsNone)
        |> List.append [
            customTerminalBinding
            customToolBinding
        ]
    let config =
        { Config.Default with
            KeyBindings = bindings
            Tools = [
                Tool.DefaultTerminal
                customTool
            ]
        }
    let model = Model.create config

    let expectedBindings =
        (
            MainBindings.Default
            // remove tool bindings except Terminal
            |> List.filter (fun kb -> not (kb.Command.ToolName |> Option.exists ((<>) Tool.DefaultTerminal.ToolName)))
        ) @ [customToolBinding]

    Settings.performResetAllKeyBindings(model).Config.KeyBindings
    |> assertAreEqual expectedBindings
